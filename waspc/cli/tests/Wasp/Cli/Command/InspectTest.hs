module Wasp.Cli.Command.InspectTest where

import Test.Hspec
import Wasp.AppSpec.Core.Inspectable (InspectionEntry (..))
import Wasp.AppSpec.Inspect (InspectionSection (..))
import Wasp.Cli.Command.Inspect (renderInspection)
import Wasp.Cli.Terminal (title)

spec_renderInspection :: Spec
spec_renderInspection = do
  it "aligns cells into columns and drops trailing whitespace" $
    renderInspection
      [ InspectionSection
          "Routes"
          [ InspectionEntry ["/", "HomeRoute", ""],
            InspectionEntry ["/login", "LoginRoute", "[auth]"]
          ]
      ]
      `shouldBe` unlines
        [ title "Routes",
          "  /       HomeRoute",
          "  /login  LoginRoute  [auth]"
        ]
  it "separates sections with a blank line and omits empty ones" $
    renderInspection
      [ InspectionSection "Routes" [InspectionEntry ["/", "HomeRoute"]],
        InspectionSection "Pages" [],
        InspectionSection "Queries" [InspectionEntry ["getTasks"]]
      ]
      `shouldBe` unlines
        [ title "Routes",
          "  /  HomeRoute",
          "",
          title "Queries",
          "  getTasks"
        ]
