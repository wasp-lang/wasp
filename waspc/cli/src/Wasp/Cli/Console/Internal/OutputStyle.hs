-- | Whether what Wasp writes can carry text styles. Only "Wasp.Cli.Console"
-- and the modules next to this one use it.
module Wasp.Cli.Console.Internal.OutputStyle
  ( OutputStyle (..),
    renderInOutputStyle,
  )
where

import qualified Wasp.Util.TextStyle as TextStyle

data OutputStyle
  = -- | For a terminal that shows colours and other text styles.
    Styled
  | -- | For everything else, e.g. a file or a pipe.
    Plain

-- | The call sites mark parts of their text with styles (e.g. code), whatever
-- the output is. This drops the styles that the output can't show.
renderInOutputStyle :: OutputStyle -> String -> String
renderInOutputStyle Styled = id
renderInOutputStyle Plain = TextStyle.stripStyles
