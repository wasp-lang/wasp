{-# LANGUAGE TupleSections #-}

module Wasp.Analyzer.TypeChecker.Monad
  ( TypeChecker,
    lookupType,
    setType,
    throw,
    lookupDeclType,
    run,
    -- Exported for testing
    Bindings,
    runWithBound,
  )
where

import Wasp.Analyzer.Parser.AST
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.TypeError
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.HashMap.Strict as H

type Bindings = H.HashMap Identifier Type

type TypeChecker a = StateT Bindings (ReaderT TD.TypeDefinitions (Except TypeError)) a

lookupType :: Identifier -> TypeChecker (Maybe Type)
lookupType ident = gets $ H.lookup ident

setType :: Identifier -> Type -> TypeChecker ()
setType ident typ = modify $ H.insert ident typ

throw :: TypeError -> TypeChecker a
throw = throwError

lookupDeclType :: String -> TypeChecker (Maybe TD.DeclType)
lookupDeclType name = asks $ TD.getDeclType name

runWithBound :: Bindings -> TD.TypeDefinitions -> TypeChecker a -> Either TypeError a
runWithBound bindings typeDefs t = runExcept $ flip runReaderT typeDefs $ evalStateT t bindings

run :: TD.TypeDefinitions -> TypeChecker a -> Either TypeError a
run typeDefs = runWithBound bindings typeDefs
  where
    enumValueBindings :: [(Identifier, Type)]
    enumValueBindings =
      concatMap
        (\(TD.EnumType name variants) -> zip variants (repeat $ EnumType name))
        $ TD.getEnumTypes typeDefs

    bindings :: Bindings
    bindings = H.fromList enumValueBindings
