module Wasp.Cli.Command.UseRecipe.Tailwind where

import Control.Monad.Cont (MonadIO (liftIO))
import StrongPath
  ( Dir,
    Path',
    Rel,
    reldir,
    relfile,
    (</>),
  )
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Cli.Command.UseRecipe.Common
  ( RecipesDir,
    copyFileIfDoesNotExist,
  )
import qualified Wasp.Message as Msg

data TailwindDir

tailwindDirInRecipesDir :: Path' (Rel RecipesDir) (Dir TailwindDir)
tailwindDirInRecipesDir = [reldir|tailwind|]

useTailwind :: Command ()
useTailwind = do
  InWaspProject waspProjectDir <- require
  let tailwindTargetDir = waspProjectDir

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
    tailwindConfigPath = [relfile|tailwind.config.cjs|]
    postcssConfigPath = [relfile|postcss.config.cjs|]
