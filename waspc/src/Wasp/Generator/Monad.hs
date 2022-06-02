{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Generator.Monad
  ( Generator,
    GeneratorError (..),
    GeneratorWarning (..),
    catchGeneratorError,
    logAndThrowGeneratorError,
    logGeneratorWarning,
    runGenerator,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import qualified Control.Monad.Except as MonadExcept
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState, StateT (runStateT), modify)
import Data.List.NonEmpty (NonEmpty, fromList)

-- | Generator is a monad transformer stack where we abstract away the underlying
-- concrete monad transformers with the helper functions below. This will allow us
-- to refactor and add more transformers (or swap them) without any caller changes.
--
-- The outer Either layer represents the last error that halted generation. Any error logged and thrown is fatal.
-- The mechanism to catch errors is only there to assist in collecting more errors, not recover.
-- There may optionally be additional errors or non-fatal warnings logged in the State.
newtype Generator a = Generator
  { _runGenerator :: ExceptT GeneratorError (StateT GeneratorState Identity) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState GeneratorState,
      MonadError GeneratorError
    )

data GeneratorState = GeneratorState
  { warnings :: [GeneratorWarning],
    errors :: [GeneratorError]
  }

data GeneratorError = GenericGeneratorError String

instance Show GeneratorError where
  show (GenericGeneratorError e) = e

data GeneratorWarning
  = GenericGeneratorWarning String
  | GeneratorNeedsMigrationWarning String

instance Show GeneratorWarning where
  show (GenericGeneratorWarning e) = e
  show (GeneratorNeedsMigrationWarning e) = e

-- Runs the generator and either returns a result, or a list of 1 or more errors.
-- Results in error if any error was ever logged and thrown (even if caught).
-- Even if successful there may be warnings, so they are always included.
runGenerator :: Generator a -> ([GeneratorWarning], Either (NonEmpty GeneratorError) a)
runGenerator generator =
  let (errorOrResult, finalState) = runIdentity $ runStateT (runExceptT (_runGenerator generator)) initialState
   in (warnings finalState, loggedErrorsOrResult (errorOrResult, errors finalState))
  where
    initialState = GeneratorState {warnings = [], errors = []}

    loggedErrorsOrResult (Right result, []) = Right result
    loggedErrorsOrResult (Left _, []) = error "Generator produced error, but had empty log - this should never happen!"
    loggedErrorsOrResult (_, loggedErrors) = Left $ fromList loggedErrors

-- This logs a warning but does not short circuit the computation.
logGeneratorWarning :: GeneratorWarning -> Generator ()
logGeneratorWarning w = modify $ \GeneratorState {errors = errors', warnings = warnings'} ->
  GeneratorState {errors = errors', warnings = w : warnings'}

-- This logs an error and does throw, thus short-circuiting the computation until caught.
logAndThrowGeneratorError :: GeneratorError -> Generator a
logAndThrowGeneratorError e = logGeneratorError >> throwError e
  where
    logGeneratorError :: Generator ()
    logGeneratorError = modify $ \GeneratorState {errors = errors', warnings = warnings'} ->
      GeneratorState {errors = e : errors', warnings = warnings'}

-- This stops the short-circuiting from above, if ever desired, but cannot be used for full recovery.
-- Once one error is logged and thrown the result will be error. This function exists to log
-- more errors on the way up.
catchGeneratorError :: Generator a -> (GeneratorError -> Generator a) -> Generator a
catchGeneratorError = MonadExcept.catchError
