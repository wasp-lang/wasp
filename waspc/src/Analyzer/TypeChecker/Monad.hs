{-# LANGUAGE TupleSections #-}

module Analyzer.TypeChecker.Monad where

import Analyzer.Parser.AST
import Analyzer.Type
import Analyzer.TypeChecker.TypeError
import qualified Analyzer.TypeDefinitions as TD
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as H

type Bindings = H.HashMap Ident Type

type T a = StateT Bindings (ReaderT TD.TypeDefinitions (Except TypeError)) a

lookupType :: Ident -> T (Maybe Type)
lookupType ident = gets $ H.lookup ident

setType :: Ident -> Type -> T ()
setType ident typ = modify $ H.insert ident typ

throw :: TypeError -> T a
throw = throwError

lookupDecl :: String -> T (Maybe TD.DeclType)
lookupDecl name = asks $ TD.getDeclType name

runTWithBound :: Bindings -> TD.TypeDefinitions -> T a -> Either TypeError a
runTWithBound bindings tds t = runExcept $ flip runReaderT tds $ evalStateT t bindings

runT :: TD.TypeDefinitions -> T a -> Either TypeError a
runT tds = runTWithBound bindings tds
  where
    bindings :: Bindings
    -- Binds all enum values to the correct enum types
    bindings =
      foldr
        (\(TD.EnumType name variants) b -> H.fromList (map (,EnumType name) variants) <> b)
        H.empty
        $ TD.getEnumTypes tds
