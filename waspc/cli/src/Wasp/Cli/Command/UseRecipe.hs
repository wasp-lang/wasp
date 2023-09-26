module Wasp.Cli.Command.UseRecipe where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (fromList)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.UseRecipe.Auth (useAuth)
import Wasp.Cli.Command.UseRecipe.EmailSender (useEmailSender)
import Wasp.Cli.Command.UseRecipe.Tailwind (useTailwind)
import qualified Wasp.Cli.Interactive as Interactive

data Recipe = Recipe
  { recipeName :: String,
    recipeDescription :: String,
    execute :: Command ()
  }

instance Show Recipe where
  show = recipeName

instance Interactive.Option Recipe where
  showOption = show
  showOptionDescription = Just . recipeDescription

useRecipe :: [String] -> Command ()
useRecipe _args = do
  InWaspProject _ <- require

  let recipes =
        [ Recipe
            { recipeName = "Tailwind",
              recipeDescription = "Add support for Tailwind CSS",
              execute = useTailwind
            },
          Recipe
            { recipeName = "Auth",
              recipeDescription = "Add authentication to your app",
              execute = useAuth
            },
          Recipe
            { recipeName = "Email sender",
              recipeDescription = "Set up an email sender",
              execute = useEmailSender
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
