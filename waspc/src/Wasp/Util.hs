{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Wasp.Util
  ( Checksum,
    camelToKebabCase,
    checksumFromString,
    checksumFromText,
    checksumFromByteString,
    onFirst,
    toLowerFirst,
    toUpperFirst,
    headSafe,
    second3,
    jsonSet,
    indent,
    concatShortPrefixAndText,
    concatPrefixAndText,
    insertAt,
    leftPad,
    (<++>),
    (<:>),
    bytestringToHex,
    hexFromString,
    hexToString,
    checksumFromFilePath,
    checksumFromChecksums,
    ifM,
    fromMaybeM,
    orIfNothing,
    orIfNothingM,
    kebabToCamelCase,
  )
where

import Control.Applicative (liftA2)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isSpace, isUpper, toLower, toUpper)
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Text.Printf (printf)

camelToKebabCase :: String -> String
camelToKebabCase "" = ""
camelToKebabCase camel@(camelHead : camelTail) = kebabHead : kebabTail
  where
    kebabHead = toLower camelHead
    kebabTail =
      concatMap
        (\(a, b) -> (if isCamelHump (a, b) then ['-'] else []) ++ [toLower b])
        (zip camel camelTail)
    isCamelHump (a, b) = (not . isUpper) a && isUpper b

kebabToCamelCase :: String -> String
kebabToCamelCase = concat . capitalizeAllWordsExceptForTheFirstOne . wordsBy (== '-')
  where
    capitalizeAllWordsExceptForTheFirstOne :: [String] -> [String]
    capitalizeAllWordsExceptForTheFirstOne [] = []
    capitalizeAllWordsExceptForTheFirstOne (firstWord : otherWords) = firstWord : map toUpperFirst otherWords

-- | Applies given function to the first element of the list.
--   If list is empty, returns empty list.
onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (x : xs) = f x : xs

toLowerFirst :: String -> String
toLowerFirst = onFirst toLower

toUpperFirst :: String -> String
toUpperFirst = onFirst toUpper

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe xs = Just (head xs)

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (x, y, z) = (x, f y, z)

jsonSet :: Text.Text -> Aeson.Value -> Aeson.Value -> Aeson.Value
jsonSet key value (Aeson.Object o) = Aeson.Object $ M.insert key value o
jsonSet _ _ _ = error "Input JSON must be an object"

indent :: Int -> String -> String
indent numSpaces = intercalate "\n" . map (toEmptyStringIfAllWhiteSpace . (indentation ++)) . splitOn "\n"
  where
    indentation = replicate numSpaces ' '
    toEmptyStringIfAllWhiteSpace str
      | all isSpace str = ""
      | otherwise = str

-- | Given a prefix and text, concatenates them in the following manner:
-- <prefix> <text_line_1>
--          <text_line_2>
--              ...
--          <text_line_N>
--
-- __Examples__
--
-- @
-- >>> putStrLn $ concatShortPrefixAndText "Log: " "Written to file foo.txt"
-- Log: Written to file foo.txt
-- @
--
-- @
-- >>> putStrLn $ concatShortPrefixAndText "Log: " "Written to file foo.txt\nWritten to file bar.txt"
-- Log: Written to file foo.txt
--      Written to file bar.txt
-- @
concatShortPrefixAndText :: String -> String -> String
concatShortPrefixAndText prefix "" = prefix
concatShortPrefixAndText prefix text =
  let (l : ls) = lines text
   in prefix ++ l ++ if null ls then "" else "\n" ++ indent (length prefix) (intercalate "\n" ls)

-- | Given a prefix and text, concatenates them in the following manner:
-- - If just one line of text:
-- <prefix> <one_and_only_line_of_text>
-- - If multiple lines of text:
-- <prefix>
--   <text_line_1>
--   <text_line_2>
--       ...
--   <text_line_N>
--
-- __Examples__
--
-- @
-- >>> putStrLn $ concatPrefixAndText "Log messages from the somelog.txt file: " "Written to file foo.txt"
-- Log messages from the somelog.txt file: Written to file foo.txt
-- @
--
-- @
-- >>> putStrLn $ concatPrefixAndText "Log messages from the somelog.txt file:" "Written to file foo.txt\nWritten to file bar.txt"
-- Log messages from the somelog.txt file:
--   Written to file foo.txt
--   Written to file bar.txt
-- @
concatPrefixAndText :: String -> String -> String
concatPrefixAndText prefix text =
  if length (lines text) <= 1 then prefix ++ text else prefix ++ "\n" ++ indent 2 text

-- | Adds given element to the start of the given list until the list is of specified length.
-- leftPad ' ' 4 "hi" == "  hi"
-- leftPad ' ' 4 "hihihi" == "hihihi"
leftPad :: a -> Int -> [a] -> [a]
leftPad padElem n list = replicate (max 0 (n - length list)) padElem ++ list

-- | Inserts a given @theInsert@ list into the given @host@ list so that @theInsert@
-- starts at index @idx@ in the @host@.
-- Example: @insertAt "hi" 2 "hoho" == "hohiho"@
insertAt :: [a] -> Int -> [a] -> [a]
insertAt theInsert idx host =
  let (before, after) = splitAt idx host
   in before ++ theInsert ++ after

infixr 5 <++>

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

infixr 5 <:>

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

type Checksum = Hex

checksumFromString :: String -> Checksum
checksumFromString = bytestringToHex . SHA256.hash . BSU.fromString

checksumFromText :: Text -> Checksum
checksumFromText = bytestringToHex . SHA256.hash . TextEncoding.encodeUtf8

checksumFromByteString :: BSU.ByteString -> Checksum
checksumFromByteString = bytestringToHex . SHA256.hash

checksumFromFilePath :: FilePath -> IO Checksum
checksumFromFilePath file = checksumFromByteString <$> B.readFile file

checksumFromChecksums :: [Checksum] -> Checksum
checksumFromChecksums = checksumFromString . concatMap (\(Hex s) -> s)

newtype Hex = Hex String
  deriving (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

bytestringToHex :: B.ByteString -> Hex
bytestringToHex = Hex . concatMap (printf "%02x") . B.unpack

hexFromString :: String -> Hex
hexFromString = Hex

hexToString :: Hex -> String
hexToString (Hex s) = s

fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM ma = (>>= maybe ma return)

orIfNothing :: Maybe a -> a -> a
orIfNothing = flip fromMaybe

orIfNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
orIfNothingM = flip fromMaybeM
