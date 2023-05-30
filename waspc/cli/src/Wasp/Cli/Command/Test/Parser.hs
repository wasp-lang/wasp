module Wasp.Cli.Command.Test.Parser (test) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Test), TestArgs (TestClient, TestServer))
import Wasp.Cli.Parser.Util (mkCommand, mkNormalCommand)

test :: Mod CommandFields CommandCall
test = mkNormalCommand "test" "Executes tests in your project." parseTest

parseTest :: Parser CommandCall
parseTest = Test <$> parseTestArgs
  where
    parseTestArgs =
      O.subparser $
        mconcat
          [ mkCommand "client" [O.progDesc "Run your app client tests.", O.forwardOptions] (TestClient <$> O.many testRestArgs),
            mkCommand "server" [O.progDesc "Run your app server tests.", O.forwardOptions] (TestServer <$> O.many testRestArgs)
          ]

testRestArgs :: Parser String
testRestArgs =
  O.strArgument $
    O.metavar "VITEST_ARGUMENTS"
      <> O.help "Extra arguments that will be passed to Vitest. See https://vitest.dev/guide/cli.html"
