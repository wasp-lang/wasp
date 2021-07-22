{-# LANGUAGE TupleSections #-}

module Analyzer.TypeChecker.Monad where

import Analyzer.Parser.AST
import Analyzer.Type
import Analyzer.TypeChecker.TypeError
import qualified Analyzer.TypeDefinitions as TD
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.HashMap.Strict as H

type Bindings = H.HashMap Ident Type

type T a = StateT Bindings (ReaderT TD.TypeDefinitions (Except TypeError)) a

lookupType :: Ident -> T (Maybe Type)
lookupType ident = gets $ H.lookup ident

setType :: Ident -> Type -> T ()
setType ident typ = modify $ H.insert ident typ

throw :: TypeError -> T a
throw = throwError

lookupDeclType :: String -> T (Maybe TD.DeclType)
lookupDeclType name = asks $ TD.getDeclType name

runTWithBound :: Bindings -> TD.TypeDefinitions -> T a -> Either TypeError a
runTWithBound bindings typeDefs t = runExcept $ flip runReaderT typeDefs $ evalStateT t bindings

runT :: TD.TypeDefinitions -> T a -> Either TypeError a
runT typeDefs = runTWithBound bindings typeDefs
  where
    enumValueBindings :: [(String, Type)]
    enumValueBindings =
      concatMap
        (\(TD.EnumType name variants) -> zip variants (repeat $ EnumType name))
        $ TD.getEnumTypes typeDefs

    bindings :: Bindings
    bindings = H.fromList enumValueBindings
