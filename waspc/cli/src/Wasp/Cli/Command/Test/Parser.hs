module Wasp.Cli.Command.Test.Parser (test) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (Test), TestArgs (TestClient, TestServer))
import Wasp.Cli.Parser.Util (mkCommand, mkNormalCommand)

test :: Mod CommandFields Call
test = mkNormalCommand "test" parseTest "Executes tests in your project."

parseTest :: Parser Call
parseTest = Test <$> parseTestArgs
  where
    parseTestArgs =
      O.subparser $
        mconcat
          [ mkCommand "client" [O.forwardOptions] (TestClient <$> O.many testRestArgs) "Run your app client tests.",
            mkCommand "server" [O.forwardOptions] (TestServer <$> O.many testRestArgs) "Run your app server tests."
          ]

testRestArgs :: Parser String
testRestArgs =
  O.strArgument $
    O.metavar "VITEST_ARGUMENTS"
      <> O.help "Extra arguments that will be passed to Vitest. See https://vitest.dev/guide/cli.html"
