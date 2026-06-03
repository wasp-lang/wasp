module Wasp.Cli.Command.Version
  ( printVersion,
    parserInfo,
  )
where

import Wasp.Cli.Command.Definition (CommandParserInfo, leaf, runWaspIO)
import Wasp.Util (indent)
import Wasp.Util.InstallMethod (getInstallationCommand)
import Wasp.Version (waspVersion)

parserInfo :: CommandParserInfo
parserInfo = leaf "Print the version of the CLI." (runWaspIO printVersion)

printVersion :: IO ()
printVersion =
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        indent 2 $ getInstallationCommand Nothing,
        "",
        "If you want a specific x.y.z version of Wasp, do:",
        indent 2 $ getInstallationCommand $ Just "x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]
