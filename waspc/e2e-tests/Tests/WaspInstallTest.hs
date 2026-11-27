module Tests.WaspInstallTest (waspInstallTest) where

import ShellCommands (assertCommandOutputContains)
import Test (Test (..), TestCase (..))
import Wasp.Util.TextStyle (styleCode)

waspInstallTest :: Test
waspInstallTest =
  Test
    "wasp-install"
    [ TestCase
        "install-fails-with-npm-install-hint"
        ( sequence
            [ assertCommandOutputContains
                (return "! $WASP_CLI_CMD install")
                ("Run " ++ styleCode "npm install" ++ " instead")
            ]
        )
    ]
