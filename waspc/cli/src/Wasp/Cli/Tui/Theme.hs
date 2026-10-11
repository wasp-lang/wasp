-- | Colors and attributes for the @wasp start@ TUI.
--
-- The palette follows the Wasp brand: brand yellow as the primary accent,
-- purple as the secondary one, and muted greys for everything that should stay
-- out of the way. Colors are 24-bit RGB; Vty downsamples them on terminals
-- that can't do truecolor.
module Wasp.Cli.Tui.Theme
  ( -- * Palette
    brandYellow,
    brandPurple,
    colorInk,
    colorMuted,
    colorFaint,
    colorRed,
    colorAmber,
    colorGreen,
    colorCyan,
    colorBlue,
    colorChipText,
    colorSurface,

    -- * Attributes
    defaultAttr,
    plainAttr,
    boldAttr,
    chipAttr,
    inkAttr,
    mutedAttr,
    faintAttr,
    surfaceAttr,
    onSurface,
    tuiAttrMap,
  )
where

import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

brandYellow :: V.Color
brandYellow = V.rgbColor (245 :: Int) 200 66

brandPurple :: V.Color
brandPurple = V.rgbColor (169 :: Int) 145 250

colorInk :: V.Color
colorInk = V.rgbColor (222 :: Int) 225 232

colorMuted :: V.Color
colorMuted = V.rgbColor (138 :: Int) 143 156

colorFaint :: V.Color
colorFaint = V.rgbColor (82 :: Int) 87 100

colorRed :: V.Color
colorRed = V.rgbColor (244 :: Int) 104 104

colorAmber :: V.Color
colorAmber = V.rgbColor (238 :: Int) 178 82

colorGreen :: V.Color
colorGreen = V.rgbColor (90 :: Int) 208 144

colorCyan :: V.Color
colorCyan = V.rgbColor (94 :: Int) 200 219

colorBlue :: V.Color
colorBlue = V.rgbColor (112 :: Int) 160 245

-- | Text drawn on top of a brand-colored chip.
colorChipText :: V.Color
colorChipText = V.rgbColor (20 :: Int) 20 23

-- | Background of panels that float above the rest of the UI.
colorSurface :: V.Color
colorSurface = V.rgbColor (31 :: Int) 32 38

defaultAttr :: V.Attr
defaultAttr = V.defAttr `V.withForeColor` colorInk

-- | Foreground color on the terminal's own background.
plainAttr :: V.Color -> V.Attr
plainAttr color = V.defAttr `V.withForeColor` color

boldAttr :: V.Color -> V.Attr
boldAttr color = plainAttr color `V.withStyle` V.bold

-- | A filled block of color with dark text on it: the Wasp wordmark, tab
-- badges, and other things that should read as a solid stamp.
chipAttr :: V.Color -> V.Attr
chipAttr color = (V.defAttr `V.withForeColor` colorChipText `V.withBackColor` color) `V.withStyle` V.bold

inkAttr :: V.Attr
inkAttr = plainAttr colorInk

mutedAttr :: V.Attr
mutedAttr = plainAttr colorMuted

faintAttr :: V.Attr
faintAttr = plainAttr colorFaint

surfaceAttr :: V.Attr
surfaceAttr = V.defAttr `V.withForeColor` colorInk `V.withBackColor` colorSurface

-- | Puts an attribute on the floating-panel background.
onSurface :: V.Attr -> V.Attr
onSurface attr = attr `V.withBackColor` colorSurface

-- | The TUI draws its own images with explicit attributes, so the attribute
-- map only needs to set the default.
tuiAttrMap :: A.AttrMap
tuiAttrMap = A.attrMap defaultAttr []
