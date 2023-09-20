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
  InWaspProject _waspProjectDir <- require

  recipe <- liftIO selectRecipe

  execute recipe
  where
    recipes =
      [ Recipe
          { recipeName = "tailwind",
            execute = useTailwind
          },
        Recipe
          { recipeName = "auth",
            execute = useAuth
          }
      ]
    selectRecipe =
      Interactive.askToChoose
        "What do you want to use?"
        (fromList recipes)
        Interactive.ChooserConfig {Interactive.hasDefaultOption = False}
