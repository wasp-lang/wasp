module Wasp.Cli.Command.Test.Parser (test) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (Test), TestArgs (TestClient, TestServer))
import Wasp.Cli.Parser.Util (CommandType (CTForwardOptions), mkCommand, mkWrapperCommand)

test :: Mod CommandFields Call
test = mkCommand "test" parseTest "Executes tests in your project."

parseTest :: Parser Call
parseTest = Test <$> parseTestArgs
  where
    parseTestArgs =
      O.subparser $
        mconcat
          [ mkWrapperCommand "client" CTForwardOptions (TestClient <$> O.many testRestArgs) "Run your app client tests.",
            mkWrapperCommand "server" CTForwardOptions (TestServer <$> O.many testRestArgs) "Run your app server tests."
          ]

testRestArgs :: Parser String
testRestArgs =
  O.strArgument $
    O.metavar "VITEST_ARGUMENTS"
      <> O.help "Extra arguments that will be passed to Vitest. See https://vitest.dev/guide/cli.html"
