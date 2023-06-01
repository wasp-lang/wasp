module Wasp.Cli.Command.Test.Parser (testParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Test), TestArgs (TestClient))
import Wasp.Cli.Parser.Util (mkCommandWithInfo)

testParser :: Parser CommandCall
testParser = Test <$> testRestArgsParser
  where
    testRestArgsParser = O.subparser $ mkCommandWithInfo "client" [O.progDesc "Run your app client tests.", O.forwardOptions] (TestClient <$> O.many testRestArgs)

testRestArgs :: Parser String
testRestArgs =
  O.strArgument $
    O.metavar "VITEST_ARGUMENTS"
      <> O.help "Extra arguments that will be passed to Vitest. See https://vitest.dev/guide/cli.html"
