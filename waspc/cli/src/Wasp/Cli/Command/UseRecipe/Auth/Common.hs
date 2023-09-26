module Wasp.Cli.Command.UseRecipe.Auth.Common where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', Rel, reldir, relfile, (</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Cli.Command.UseRecipe.Common (RecipesDir, copyFileIfDoesNotExist)
import Wasp.Cli.Common (WaspProjectDir)

data AuthRecipeDir

authDirInRecipesDir :: Path' (Rel RecipesDir) (Dir AuthRecipeDir)
authDirInRecipesDir = [reldir|auth|]

copyLoginPage :: Command ()
copyLoginPage = do
  authTargetDir <- getAuthTargetDir
  liftIO $ copyFileIfDoesNotExist (authDirInRecipesDir </> loginPagePath) (authTargetDir </> loginPagePath)
  where
    loginPagePath = [relfile|LoginPage.jsx|]

copySignupPage :: Command ()
copySignupPage = do
  authTargetDir <- getAuthTargetDir
  liftIO $ copyFileIfDoesNotExist (authDirInRecipesDir </> signupPagePath) (authTargetDir </> signupPagePath)
  where
    signupPagePath = [relfile|SignupPage.jsx|]

copyRequestPasswordResetPage :: Command ()
copyRequestPasswordResetPage = do
  authTargetDir <- getAuthTargetDir
  liftIO $ copyFileIfDoesNotExist (authDirInRecipesDir </> requestPasswordResetPagePath) (authTargetDir </> requestPasswordResetPagePath)
  where
    requestPasswordResetPagePath = [relfile|RequestPasswordResetPage.jsx|]

copyResetPasswordPage :: Command ()
copyResetPasswordPage = do
  authTargetDir <- getAuthTargetDir
  liftIO $ copyFileIfDoesNotExist (authDirInRecipesDir </> resetPasswordPagePath) (authTargetDir </> resetPasswordPagePath)
  where
    resetPasswordPagePath = [relfile|ResetPasswordPage.jsx|]

copyEmailVerificationPage :: Command ()
copyEmailVerificationPage = do
  authTargetDir <- getAuthTargetDir
  liftIO $ copyFileIfDoesNotExist (authDirInRecipesDir </> emailVerificationPagePath) (authTargetDir </> emailVerificationPagePath)
  where
    emailVerificationPagePath = [relfile|EmailVerificationPage.jsx|]

showRouteAndPageDSL :: String -> String -> String -> String
showRouteAndPageDSL routeName path pageName = unlines [route, page]
  where
    route = "route " <> routeName <> " { path: \"" <> path <> "\", to: " <> pageName <> " }"
    page = "page " <> pageName <> " { component: import { " <> pageName <> " } from \"@client/auth/" <> pageName <> ".jsx\" }"

getAuthTargetDir :: Command (Path' Abs (Dir WaspProjectDir))
getAuthTargetDir = do
  InWaspProject waspProjectDir <- require
  let authTargetDir = waspProjectDir </> [reldir|src/client/auth|]
  return authTargetDir
