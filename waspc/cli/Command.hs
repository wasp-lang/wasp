{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Command
    ( Command
    , runCommand
    , CommandError(..)
    ) where

import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)


newtype Command a = Command { _runCommand :: ExceptT CommandError IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError)

runCommand :: Command a -> IO ()
runCommand cmd = do
    errorOrResult <- runExceptT $ _runCommand cmd
    case errorOrResult of
        Left cmdError -> putStrLn $ "Error: " ++ _errorMsg cmdError
        Right _ -> return ()

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError { _errorMsg :: !String }

