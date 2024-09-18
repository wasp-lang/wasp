module Wasp.Cli.Command.TsConfigSetup (tsConfigSetup) where

import Control.Concurrent (newChan)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)

tsConfigSetup :: Command ()
tsConfigSetup = do
  InWaspProject waspProjectDir <- require
  messageChan <- liftIO newChan
  liftIO $
    installProjectNpmDependencies messageChan waspProjectDir >>= \case
      Left e -> putStrLn $ "npm install failed: " ++ show e
      Right _ -> return ()

-- liftIO $ copyTsSdkLib waspProjectDir

-- TODO: Edit package.json to contain the SDK package