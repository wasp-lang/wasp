-- | Rendering of the @wasp start@ TUI.
--
-- The widgets here build Vty images directly instead of composing brick's
-- higher level widgets. Logs need exact control over wrapping and over which
-- rows are on screen (the scroll position is counted in wrapped rows, not in
-- log lines), and once the log pane is hand-drawn it is simpler and more
-- consistent to draw the chrome the same way.
module Wasp.Cli.Tui.Draw
  ( drawTui,
  )
where

import Brick (Widget (..))
import Brick.Types (Result (..), Size (..), availHeight, availWidth, emptyResult, getContext)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Exit (ExitCode (..))
import Wasp.Cli.Tui.Ansi (Segment (..))
import Wasp.Cli.Tui.Layout (LogRow (..), logContentWidthFor, logGutterWidth, visibleLogRows)
import Wasp.Cli.Tui.LogBuffer
  ( LogLevel (..),
    LogLine (..),
    numBufferedLines,
    numDroppedLines,
  )
import Wasp.Cli.Tui.Model
  ( TuiName (..),
    TuiState (..),
    activePane,
    formatUptime,
    logBodyHeightFor,
    orderedPanes,
    spinnerFrame,
  )
import Wasp.Cli.Tui.Pane
  ( Pane (..),
    PaneId (..),
    PaneStatus (..),
    paneIsBusy,
    paneStatusLabel,
  )
import qualified Wasp.Cli.Tui.Theme as Theme

drawTui :: TuiState -> [Widget TuiName]
drawTui state = [drawHelpOverlay | tuiShowHelp state] ++ [mainLayer]
  where
    mainLayer = fullScreen $ \width height ->
      V.vertCat $
        concat
          [ [drawHeaderRow state width],
            [drawUrlsRow state width],
            [blankRow width],
            drawTabBar state width,
            [drawLogBody state width (logBodyHeightFor height)],
            [drawStatusRow state width],
            [drawFooterRow state width]
          ]

fullScreen :: (Int -> Int -> V.Image) -> Widget TuiName
fullScreen makeImage = Widget Greedy Greedy $ do
  context <- getContext
  return emptyResult {image = makeImage (availWidth context) (availHeight context)}

-- -----------------------------------------------------------------------------
-- Header
-- -----------------------------------------------------------------------------

-- | @  WASP   todoapp                            ⠙  2 starting · 1m 04s@
drawHeaderRow :: TuiState -> Int -> V.Image
drawHeaderRow state width = leftRight width Theme.faintAttr ' ' left right
  where
    left =
      V.horizCat
        [ text Theme.faintAttr " ",
          text (Theme.chipAttr Theme.brandYellow) " WASP ",
          text Theme.inkAttr ("  " <> tuiProjectName state)
        ]

    right =
      V.horizCat
        [ overallStatusImage state,
          text Theme.mutedAttr ("   " <> formatUptime state <> " ")
        ]

-- | A one-glance summary of how the whole app is doing.
overallStatusImage :: TuiState -> V.Image
overallStatusImage state
  | numFailed > 0 =
      V.horizCat
        [ text (Theme.boldAttr Theme.colorRed) "■ ",
          text (Theme.plainAttr Theme.colorRed) $
            tshow numFailed <> (if numFailed == 1 then " process down" else " processes down")
        ]
  | numBusy > 0 =
      V.horizCat
        [ text (Theme.boldAttr Theme.brandYellow) (spinnerFrame (tuiTickCount state) <> " "),
          text (Theme.plainAttr Theme.brandYellow) "starting up"
        ]
  -- No marker when everything is fine: the words say it, and a healthy screen
  -- should be quiet.
  | numReady > 0 = text (Theme.plainAttr Theme.colorGreen) "all systems go"
  | otherwise = text Theme.faintAttr "waiting"
  where
    statuses = map paneStatus (orderedPanes state)
    numFailed = length [() | s <- statuses, isFailureStatus s]
    numBusy = length [() | s <- statuses, paneIsBusy s]
    numReady = length [() | s <- statuses, s == StatusReady]

isFailureStatus :: PaneStatus -> Bool
isFailureStatus = \case
  StatusFailed -> True
  StatusExited (ExitFailure _) -> True
  _ -> False

-- | @  client  http://localhost:3000/    server  http://localhost:3001/@
drawUrlsRow :: TuiState -> Int -> V.Image
drawUrlsRow state width =
  fitWidth width . V.horizCat $
    text Theme.faintAttr " " : imagesThatFit (width - 1) (map urlImage (tuiAppUrls state))
  where
    urlImage (label, url) =
      V.horizCat
        [ text Theme.mutedAttr (" " <> label <> "  "),
          text (Theme.plainAttr Theme.colorCyan) url,
          text Theme.faintAttr "   "
        ]

-- -----------------------------------------------------------------------------
-- Tab bar
-- -----------------------------------------------------------------------------

-- | Two rows: the tab labels, and a rule across the whole width that thickens
-- into the accent color under the active tab.
drawTabBar :: TuiState -> Int -> [V.Image]
drawTabBar state width = [fitWidth width labelsRow, fitWidth width underlineRow]
  where
    tabs = zip [1 :: Int ..] (orderedPanes state)
    rendered = map renderTab tabs

    labelsRow = V.horizCat $ text Theme.faintAttr " " : map fst rendered
    -- The rule continues past the last tab to the edge of the screen, which
    -- makes it read as one line separating the tabs from their content.
    underlineRow =
      V.horizCat $
        [text Theme.faintAttr " "]
          ++ map snd rendered
          ++ [V.charFill Theme.faintAttr '─' (max 0 (width - 1 - sum (map (V.imageWidth . snd) rendered))) (1 :: Int)]

    renderTab (index, pane) = (labelImage, underlineImage)
      where
        isActive = paneId pane == tuiActivePaneId state
        labelAttr =
          if isActive
            then Theme.boldAttr (paneAccent pane)
            else Theme.plainAttr Theme.colorMuted

        labelImage =
          V.horizCat $
            concat
              [ [text Theme.faintAttr "  "],
                [statusGlyphImage state pane],
                [text Theme.faintAttr " "],
                [text (Theme.plainAttr Theme.colorFaint) (T.pack (show index) <> " ")],
                [text labelAttr (paneLabel pane)],
                badgeImages pane,
                [text (Theme.boldAttr Theme.colorAmber) " •" | paneHasUnseenProblems pane && not isActive],
                [text Theme.faintAttr "  "]
              ]

        underlineImage =
          V.charFill
            (Theme.plainAttr (if isActive then paneAccent pane else Theme.colorFaint))
            (if isActive then '━' else '─')
            (V.imageWidth labelImage)
            (1 :: Int)

    -- At most one badge per tab: the error count if there is one, otherwise
    -- the warning count. Showing both makes the tab bar too busy to scan.
    badgeImages pane = concatMap (\badge -> [text Theme.faintAttr " ", badge]) badges
      where
        badges
          | paneNumErrors pane > 0 =
              [text (Theme.chipAttr Theme.colorRed) (" " <> tshow (paneNumErrors pane) <> " ")]
          | paneNumWarnings pane > 0 =
              [text (Theme.chipAttr Theme.colorAmber) (" " <> tshow (paneNumWarnings pane) <> " ")]
          | otherwise = []

-- | A spinner while the process is working, a colored dot otherwise.
-- | The glyph in front of a tab's name.
--
-- A tab that is simply up and serving gets no glyph: that is the state every
-- tab is in most of the time, and marking it everywhere turns the marks into
-- wallpaper. The column is still reserved, so tabs don't jump around as
-- statuses change.
statusGlyphImage :: TuiState -> Pane -> V.Image
statusGlyphImage state pane = case paneStatus pane of
  StatusReady -> blank
  StatusIdle -> text Theme.faintAttr "○"
  StatusStarting -> spinner (paneAccent pane)
  StatusWorking -> spinner Theme.brandYellow
  StatusFailed -> text (Theme.boldAttr Theme.colorRed) "■"
  StatusExited ExitSuccess -> text Theme.mutedAttr "◌"
  StatusExited (ExitFailure _) -> text (Theme.boldAttr Theme.colorRed) "■"
  where
    spinner color = text (Theme.boldAttr color) (spinnerFrame (tuiTickCount state))
    blank = V.charFill Theme.defaultAttr ' ' (1 :: Int) (1 :: Int)

-- -----------------------------------------------------------------------------
-- Log body
-- -----------------------------------------------------------------------------

drawLogBody :: TuiState -> Int -> Int -> V.Image
drawLogBody state width height
  | null rows = drawEmptyPane pane width height
  | otherwise = V.vertCat $ take height $ map rowImage rows ++ repeat (blankRow width)
  where
    pane = activePane state
    (rows, _) = visibleLogRows (logContentWidthFor width) height pane

    rowImage row =
      fitWidth width $
        V.horizCat
          [ if logRowIsLineStart row
              then text (gutterAttr row) "▏"
              else V.charFill Theme.defaultAttr ' ' (1 :: Int) (1 :: Int),
            V.charFill Theme.defaultAttr ' ' (logGutterWidth - 1) (1 :: Int),
            V.horizCat (map segmentImage (styledSegments row))
          ]

    gutterAttr row =
      Theme.plainAttr $
        if logLineIsStderr (logRowLine row) && logLineLevel (logRowLine row) == LevelError
          then Theme.colorRed
          else Theme.colorFaint

    segmentImage segment = text (segmentAttr segment) (segmentText segment)

    -- A line that colors itself with ANSI codes is shown as its author meant
    -- it; everything else is colored by its detected level.
    styledSegments row =
      let line = logRowLine row
       in if logLineIsSelfColored line
            then logRowSegments row
            else [segment {segmentAttr = levelAttr (logLineLevel line)} | segment <- logRowSegments row]

-- | What a tab shows before it has any output: a hint about what will land
-- here, rather than a blank screen that looks broken.
drawEmptyPane :: Pane -> Int -> Int -> V.Image
drawEmptyPane pane width height =
  V.vertCat
    $ take height
    $ concat
      [ replicate topPadding (blankRow width),
        [centered (text Theme.mutedAttr (paneEmptyHint pane))],
        repeat (blankRow width)
      ]
  where
    topPadding = max 0 ((height - 1) `div` 2)
    centered content =
      fitWidth width $
        V.charFill Theme.defaultAttr ' ' (max 0 ((width - V.imageWidth content) `div` 2)) (1 :: Int)
          V.<|> content

paneEmptyHint :: Pane -> T.Text
paneEmptyHint pane = case paneId pane of
  PaneWasp -> "Wasp's compiler output shows up here."
  PaneServer -> "Waiting for the server to say something..."
  PaneClient -> "Waiting for the client to say something..."
  PaneDb -> "Database output (migrations, seeds, studio) shows up here."

levelAttr :: LogLevel -> V.Attr
levelAttr = \case
  LevelError -> Theme.plainAttr Theme.colorRed
  LevelWarn -> Theme.plainAttr Theme.colorAmber
  LevelSuccess -> Theme.plainAttr Theme.colorGreen
  LevelInfo -> Theme.plainAttr Theme.colorCyan
  LevelNormal -> Theme.inkAttr

-- -----------------------------------------------------------------------------
-- Status and footer
-- -----------------------------------------------------------------------------

drawStatusRow :: TuiState -> Int -> V.Image
drawStatusRow state width = leftRight width Theme.faintAttr '─' left right
  where
    pane = activePane state
    totalLines = numBufferedLines (paneBuffer pane)
    dropped = numDroppedLines (paneBuffer pane)
    (_, hiddenBelow) = visibleLogRows (logContentWidthFor width) (tuiLogBodyHeight state) pane

    left =
      V.horizCat
        [ text Theme.faintAttr "─ ",
          text (Theme.boldAttr (paneAccent pane)) (T.toUpper (paneLabel pane)),
          text Theme.faintAttr "  ",
          text (statusAttr (paneStatus pane)) (paneStatusLabel pane),
          text Theme.faintAttr " "
        ]

    right =
      V.horizCat $
        [text Theme.faintAttr " "]
          ++ intersperse (text Theme.faintAttr " · ") rightParts
          ++ [text Theme.faintAttr " ─"]

    rightParts =
      concat
        [ [text Theme.mutedAttr countedLines],
          [ text (Theme.plainAttr Theme.brandYellow) ("↓ " <> tshow hiddenBelow <> " below")
          | hiddenBelow > 0
          ],
          [ if paneIsFollowing pane
              then text (Theme.plainAttr Theme.colorGreen) "following"
              else text (Theme.boldAttr Theme.brandYellow) "paused"
          ]
        ]

    -- A "+" hints that we are no longer keeping the oldest lines.
    countedLines =
      tshow totalLines
        <> (if dropped > 0 then "+" else "")
        <> (if totalLines == 1 then " line" else " lines")

statusAttr :: PaneStatus -> V.Attr
statusAttr = \case
  StatusIdle -> Theme.faintAttr
  StatusStarting -> Theme.plainAttr Theme.brandYellow
  StatusWorking -> Theme.plainAttr Theme.brandYellow
  StatusReady -> Theme.plainAttr Theme.colorGreen
  StatusFailed -> Theme.plainAttr Theme.colorRed
  StatusExited ExitSuccess -> Theme.mutedAttr
  StatusExited (ExitFailure _) -> Theme.plainAttr Theme.colorRed

drawFooterRow :: TuiState -> Int -> V.Image
drawFooterRow state width
  | tuiIsQuitArmed state =
      fitWidth width $
        V.horizCat
          [ text (Theme.chipAttr Theme.brandYellow) "  q  ",
            text Theme.inkAttr " again to stop the app, ",
            text (Theme.boldAttr Theme.brandYellow) "any other key",
            text Theme.inkAttr " to keep it running"
          ]
  | otherwise = fitWidth width $ V.horizCat (text Theme.faintAttr " " : imagesThatFit (width - 1) keyImages)
  where
    -- Listed most useful first, since a narrow terminal drops the tail.
    keyImages =
      [ keyImage key description
      | (key, description) <-
          [ ("tab", "switch"),
            ("↑↓", "scroll"),
            ("g/G", "top/bottom"),
            ("f", if paneIsFollowing (activePane state) then "pause" else "follow"),
            ("o", "raw logs"),
            ("c", "clear"),
            ("?", "help"),
            ("q", "quit")
          ]
      ]

    keyImage key description =
      V.horizCat
        [ text (Theme.boldAttr Theme.brandYellow) ("  " <> key),
          text Theme.mutedAttr (" " <> description)
        ]

-- -----------------------------------------------------------------------------
-- Help overlay
-- -----------------------------------------------------------------------------

drawHelpOverlay :: Widget TuiName
drawHelpOverlay = Widget Greedy Greedy $ do
  context <- getContext
  let width = availWidth context
      height = availHeight context
      panelWidth = min (width - 4) 56
      body = helpPanelImage panelWidth
      panelHeight = V.imageHeight body
      left = max 0 ((width - panelWidth) `div` 2)
      top = max 0 ((height - panelHeight) `div` 2)
  return emptyResult {image = V.translate left top body}

helpPanelImage :: Int -> V.Image
helpPanelImage width =
  V.vertCat $
    concat
      [ [borderLine '┌' '┐'],
        [ panelRow $
            V.horizCat
              [ text (Theme.chipAttr Theme.brandYellow) " WASP ",
                text (onSurface Theme.inkAttr) "  keyboard shortcuts"
              ]
        ],
        [panelRow emptyLine],
        map shortcutRow shortcuts,
        [panelRow emptyLine],
        [panelRow $ text (onSurface Theme.mutedAttr) "Quitting stops the server and the client."],
        [borderLine '└' '┘']
      ]
  where
    inner = max 1 (width - 2)
    emptyLine = text surfaceAttr ""

    -- The panel sits on its own slightly lighter background so that it reads
    -- as floating above the logs rather than printed into them.
    surfaceAttr = Theme.surfaceAttr
    onSurface = Theme.onSurface
    borderAttr = onSurface Theme.faintAttr

    borderLine leftCorner rightCorner =
      V.horizCat
        [ text borderAttr (T.singleton leftCorner),
          V.charFill borderAttr '─' inner (1 :: Int),
          text borderAttr (T.singleton rightCorner)
        ]

    panelRow content =
      V.horizCat
        [ text borderAttr "│",
          fitWidthWith surfaceAttr inner (V.horizCat [text surfaceAttr " ", content]),
          text borderAttr "│"
        ]

    shortcutRow (keys, description) =
      panelRow $
        V.horizCat
          [ fitWidthWith surfaceAttr 17 (text (onSurface (Theme.boldAttr Theme.brandYellow)) keys),
            text (onSurface Theme.mutedAttr) description
          ]

    shortcuts =
      [ ("tab / shift-tab", "next / previous tab"),
        ("1 .. 4", "jump to a tab"),
        ("↑ ↓ / k j", "scroll a line"),
        ("pgup / pgdn", "scroll a page"),
        ("g / G", "jump to oldest / latest"),
        ("f", "follow the latest output"),
        ("o", "open this tab's log in your pager,"),
        ("", "where you can copy and search it"),
        ("c", "clear this tab's logs"),
        ("?", "toggle this help"),
        ("q / ctrl-c", "quit wasp start")
      ]

-- -----------------------------------------------------------------------------
-- Image helpers
-- -----------------------------------------------------------------------------

text :: V.Attr -> T.Text -> V.Image
text = V.text'

-- | As many of the images as fit in the given width, in order. Dropping a
-- whole item reads better on a narrow terminal than cutting one in half.
imagesThatFit :: Int -> [V.Image] -> [V.Image]
imagesThatFit width = go width
  where
    go _ [] = []
    go remaining (image : rest)
      | V.imageWidth image <= remaining = image : go (remaining - V.imageWidth image) rest
      | otherwise = []

blankRow :: Int -> V.Image
blankRow width = V.charFill Theme.defaultAttr ' ' width (1 :: Int)

-- | Pads with spaces or crops so the image is exactly the given width.
fitWidth :: Int -> V.Image -> V.Image
fitWidth = fitWidthWith Theme.defaultAttr

fitWidthWith :: V.Attr -> Int -> V.Image -> V.Image
fitWidthWith padAttr width image
  | V.imageWidth image >= width = V.cropRight width image
  | otherwise = image V.<|> V.charFill padAttr ' ' (width - V.imageWidth image) (1 :: Int)

-- | A row with content pushed to both edges and the given character filling
-- the gap. When there isn't room for both, the left side wins.
leftRight :: Int -> V.Attr -> Char -> V.Image -> V.Image -> V.Image
leftRight width fillAttr fillChar left right
  | gap >= 1 = V.horizCat [left, V.charFill fillAttr fillChar gap (1 :: Int), right]
  | otherwise = fitWidth width left
  where
    gap = width - V.imageWidth left - V.imageWidth right

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
