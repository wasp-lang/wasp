module Wasp.Cli.Command.UseRecipe.Tailwind where

import Control.Monad.Cont (MonadIO (liftIO))
import StrongPath
  ( Abs,
    Dir,
    Path',
    Rel,
    reldir,
    relfile,
    (</>),
  )
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.UseRecipe.Common
  ( RecipesDir,
    copyFileIfDoesNotExist,
  )
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Message as Msg

data TailwindDir

tailwindDirInRecipesDir :: Path' (Rel RecipesDir) (Dir TailwindDir)
tailwindDirInRecipesDir = [reldir|tailwind|]

useTailwind :: Path' Abs (Dir WaspProjectDir) -> Command ()
useTailwind waspProjectDir = do
  cliSendMessageC $ Msg.Start "Setting up Tailwind config files..."

  liftIO $ do
    copyFileIfDoesNotExist (tailwindDirInRecipesDir </> tailwindConfigPath) (tailwindTargetDir </> tailwindConfigPath)
    copyFileIfDoesNotExist (tailwindDirInRecipesDir </> postcssConfigPath) (tailwindTargetDir </> postcssConfigPath)

  cliSendMessageC $
    Msg.Info $
      unlines
        [ "Make sure the following to your Main.css file:",
          "",
          "@tailwind base;",
          "@tailwind components;",
          "@tailwind utilities;",
          "",
          "You can now use Tailwind classes in your CSS files."
        ]
  where
    tailwindTargetDir = waspProjectDir

    tailwindConfigPath = [relfile|tailwind.config.cjs|]
    postcssConfigPath = [relfile|postcss.config.cjs|]
