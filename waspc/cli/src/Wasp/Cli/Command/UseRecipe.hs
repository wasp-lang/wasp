module Wasp.Cli.Command.UseRecipe where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (fromList)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.UseRecipe.Auth (useAuth)
import Wasp.Cli.Command.UseRecipe.Tailwind (useTailwind)
import qualified Wasp.Cli.Interactive as Interactive

data Recipe = Recipe
  { recipeName :: String,
    execute :: Command ()
  }

instance Show Recipe where
  show = recipeName

instance Interactive.Option Recipe where
  showOption = show
  showOptionDescription _ = Nothing

useRecipe :: [String] -> Command ()
useRecipe _args = do
  InWaspProject waspProjectDir <- require

  let recipes =
        [ Recipe
            { recipeName = "tailwind",
              execute = useTailwind waspProjectDir
            },
          Recipe
            { recipeName = "auth",
              execute = useAuth
            }
        ]

  recipe <- liftIO $ selectRecipe recipes

  execute recipe
  where
    selectRecipe recipes =
      Interactive.askToChoose
        "What do you want to use?"
        (fromList recipes)
        Interactive.ChooserConfig {Interactive.hasDefaultOption = False}
