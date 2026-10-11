-- | Turning raw process output (which is full of ANSI escape sequences) into
-- styled text segments that Vty can draw.
--
-- Dev servers like Vite and Nodemon color their output with SGR escape
-- sequences. We parse those into Vty attributes so the TUI shows the logs the
-- way the user would see them in a plain terminal, and we drop every other
-- escape sequence (cursor movement, screen clearing, OSC titles, ...) since
-- those only make sense for a terminal we are not giving the process.
module Wasp.Cli.Tui.Ansi
  ( Segment (..),
    parseAnsi,
    segmentsToText,
    segmentsHaveColor,
  )
where

import Data.Bits (complement, (.&.), (.|.))
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Graphics.Vty as V

-- | A run of text that shares a single attribute.
data Segment = Segment
  { segmentAttr :: !V.Attr,
    segmentText :: !T.Text
  }

segmentsToText :: [Segment] -> T.Text
segmentsToText = T.concat . map segmentText

-- | Whether any segment carries an explicit foreground color. We use this to
-- decide whether a log line already colors itself (in which case we leave it
-- alone) or whether we should color it by detected log level.
segmentsHaveColor :: [Segment] -> Bool
segmentsHaveColor = any (hasForeColor . segmentAttr)
  where
    hasForeColor attr = case V.attrForeColor attr of
      V.SetTo _ -> True
      _ -> False

-- | Parses text that may contain ANSI escape sequences into styled segments.
--
-- Takes the attribute left over from the previous chunk of the same stream
-- (escape sequences routinely straddle chunk boundaries) and returns the
-- attribute that the next chunk should start with.
--
-- The input is expected to be a single line: newlines are not treated
-- specially.
parseAnsi :: V.Attr -> T.Text -> (V.Attr, [Segment])
parseAnsi startAttr = go startAttr []
  where
    go attr acc text
      | T.null text = (attr, reverse acc)
      | otherwise =
          let (plain, rest) = T.break isControl text
              acc' = if T.null plain then acc else Segment attr plain : acc
           in case T.uncons rest of
                Nothing -> (attr, reverse acc')
                Just ('\ESC', _) -> case takeEscapeSequence rest of
                  Nothing -> go attr acc' (T.drop 1 rest)
                  Just (escapeSequence, rest') -> go (applyEscapeSequence attr escapeSequence) acc' rest'
                -- Lone control characters (e.g. the backspaces npm uses for
                -- its progress bars) would corrupt the layout, so we drop them.
                Just (_, rest') -> go attr acc' rest'

    isControl c = c == '\ESC' || (c < ' ' && c /= '\t')

-- | Splits off the escape sequence at the start of the given text.
--
-- Returns 'Nothing' for a sequence that is cut off at the end of the text; the
-- caller then drops the lone @ESC@, which is the best we can do without
-- buffering across chunks.
takeEscapeSequence :: T.Text -> Maybe (T.Text, T.Text)
takeEscapeSequence text = case T.unpack (T.take 2 text) of
  -- CSI: ESC [ <params> <intermediates> <final byte>
  ['\ESC', '['] ->
    let afterCsi = T.drop 2 text
        (params, afterParams) = T.span isParameterByte afterCsi
        (intermediates, afterIntermediates) = T.span isIntermediateByte afterParams
     in case T.uncons afterIntermediates of
          Just (finalByte, rest)
            | isFinalByte finalByte ->
                Just (T.concat ["\ESC[", params, intermediates, T.singleton finalByte], rest)
          _ -> Nothing
  -- OSC: ESC ] <text> (BEL | ESC \)
  ['\ESC', ']'] ->
    let afterOsc = T.drop 2 text
        (body, rest) = T.break (\c -> c == '\BEL' || c == '\ESC') afterOsc
     in case T.uncons rest of
          Just ('\BEL', rest') -> Just (body, rest')
          Just ('\ESC', rest') -> Just (body, T.drop 1 rest')
          _ -> Nothing
  -- Everything else is a two-character sequence (charset designators and
  -- friends), which we simply consume and ignore.
  ['\ESC', _] -> Just (T.take 2 text, T.drop 2 text)
  _ -> Nothing
  where
    isParameterByte c = c >= '\x30' && c <= '\x3F'
    isIntermediateByte c = c >= '\x20' && c <= '\x2F'
    isFinalByte c = c >= '\x40' && c <= '\x7E'

-- | Applies an escape sequence to the current attribute. Only SGR sequences
-- (@ESC [ ... m@) change anything; the rest are ignored.
applyEscapeSequence :: V.Attr -> T.Text -> V.Attr
applyEscapeSequence attr sequenceText
  | not ("\ESC[" `T.isPrefixOf` sequenceText) = attr
  | not ("m" `T.isSuffixOf` sequenceText) = attr
  | otherwise = applySgrCodes attr codes
  where
    body = T.dropEnd 1 $ T.drop 2 sequenceText
    -- An empty parameter list means "reset", same as "0".
    codes = if T.null body then [0] else map parseCode (T.splitOn ";" body)
    parseCode t = if T.all isDigit t && not (T.null t) then read (T.unpack t) else 0

applySgrCodes :: V.Attr -> [Int] -> V.Attr
applySgrCodes = go
  where
    go attr [] = attr
    go attr (code : rest) = case code of
      0 -> go V.defAttr rest
      1 -> go (attr `V.withStyle` V.bold) rest
      2 -> go (attr `V.withStyle` V.dim) rest
      3 -> go (attr `V.withStyle` V.italic) rest
      4 -> go (attr `V.withStyle` V.underline) rest
      7 -> go (attr `V.withStyle` V.reverseVideo) rest
      9 -> go (attr `V.withStyle` V.strikethrough) rest
      22 -> go (removeStyle (V.bold .|. V.dim) attr) rest
      23 -> go (removeStyle V.italic attr) rest
      24 -> go (removeStyle V.underline attr) rest
      27 -> go (removeStyle V.reverseVideo attr) rest
      29 -> go (removeStyle V.strikethrough attr) rest
      39 -> go (attr {V.attrForeColor = V.Default}) rest
      49 -> go (attr {V.attrBackColor = V.Default}) rest
      38 -> withExtendedColor V.withForeColor attr rest
      48 -> withExtendedColor V.withBackColor attr rest
      _
        | code >= 30 && code <= 37 -> go (attr `V.withForeColor` isoColor (code - 30)) rest
        | code >= 40 && code <= 47 -> go (attr `V.withBackColor` isoColor (code - 40)) rest
        | code >= 90 && code <= 97 -> go (attr `V.withForeColor` isoColor (code - 90 + 8)) rest
        | code >= 100 && code <= 107 -> go (attr `V.withBackColor` isoColor (code - 100 + 8)) rest
        | otherwise -> go attr rest

    -- Both 38 and 48 take their color from the codes that follow them:
    -- "5;n" for a 256-color index, "2;r;g;b" for a truecolor value.
    withExtendedColor setColor attr = \case
      (5 : n : rest) -> go (setColor attr (xterm256Color n)) rest
      (2 : r : g : b : rest) -> go (setColor attr (V.rgbColor r g b)) rest
      rest -> go attr rest

    isoColor n = V.ISOColor (fromIntegral n)

    xterm256Color n
      | n < 16 = V.ISOColor (fromIntegral n)
      | n < 256 = V.Color240 (fromIntegral (n - 16))
      | otherwise = V.ISOColor 7

removeStyle :: V.Style -> V.Attr -> V.Attr
removeStyle style attr = case V.attrStyle attr of
  V.SetTo current -> attr {V.attrStyle = V.SetTo (current .&. complement style)}
  _ -> attr
