module Wasp.Cli.Util.EnvVarsTest where

import Test.Hspec
import Wasp.Cli.Util.EnvVars (findWaspOwnedEnvVarsSetByUser)
import Wasp.Project.Apps (Apps (..))

spec_findWaspOwnedEnvVarsSetByUser :: Spec
spec_findWaspOwnedEnvVarsSetByUser = do
  describe "findWaspOwnedEnvVarsSetByUser" $ do
    it "finds nothing when the user set no wasp-owned vars" $ do
      findWaspOwnedEnvVarsSetByUser
        (Apps {client = ["PORT"], server = ["PORT", "WASP_SERVER_URL"]})
        ( Apps
            { client = [(".env.client", ["SOME_VAR"])],
              server = [(".env.server", [])]
            }
        )
        `shouldBe` []

    it "reports each var with the source it was set in, sorted by name" $ do
      findWaspOwnedEnvVarsSetByUser
        (Apps {client = ["PORT", "REACT_APP_API_URL"], server = ["PORT"]})
        ( Apps
            { client = [(".env.client", ["REACT_APP_API_URL"])],
              server = [(".env.server", ["PORT", "SOME_VAR"]), ("your environment", [])]
            }
        )
        `shouldBe` [ ("PORT", [".env.server"]),
                     ("REACT_APP_API_URL", [".env.client"])
                   ]

    it "merges a var set for both apps into one entry listing all its sources" $ do
      findWaspOwnedEnvVarsSetByUser
        (Apps {client = ["PORT"], server = ["PORT"]})
        ( Apps
            { client = [(".env.client", ["PORT"])],
              server = [(".env.server", ["PORT"]), ("your environment", ["PORT"])]
            }
        )
        `shouldBe` [("PORT", [".env.client", ".env.server", "your environment"])]
