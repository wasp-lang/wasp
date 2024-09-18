module Wasp.Cli.Command.TsConfigSetup (tsConfigSetup) where

import Control.Concurrent (newChan)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)

-- | Prepares the project for using Wasp's TypeScript SDK.
tsConfigSetup :: Command ()
tsConfigSetup = do
  InWaspProject waspProjectDir <- require
  messageChan <- liftIO newChan
  -- TODO: Both of these should be eaiser when Miho finishes the package.json
  -- and tsconfig.json validation:
  -- - Edit package.json to contain the SDK package
  -- - Adapt TSconfigs to fit with the TS SDK project structure
  liftIO $
    -- NOTE: We're only installing the user's package.json dependencies here
    -- This is to provide proper IDE support for users working with the TS SDK
    -- (it needs the `wasp-config` package).
    -- Calling this function here shouldn't break anything for later
    -- installations.
    -- TODO: What about doing this during Wasp start? Can we make Wasp start
    -- pick up whether the user wants to use the TS SDK automatically?
    installProjectNpmDependencies messageChan waspProjectDir >>= \case
      Left e -> putStrLn $ "npm install failed: " ++ show e
      Right _ -> return ()
