module Wasp.Cli.Util.Parser
  ( renderParserHelp,
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt.Help
import Options.Applicative.Help.Core (parserHelp)

-- | Renders a parser's help text (its usage and options) as a String, e.g. to
-- embed in an error message.
renderParserHelp :: Opt.Parser a -> String
renderParserHelp = Opt.Help.renderHelp (Opt.prefColumns Opt.defaultPrefs) . parserHelp Opt.defaultPrefs
