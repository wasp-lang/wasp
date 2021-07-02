module Analyzer.TypeChecker.Internal where

import Analyzer.Parser.AST
import Analyzer.Type
import Analyzer.TypeChecker.AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as H

type Bindings = H.HashMap Ident Type

type T a = StateT Bindings (Except TypeError) a

lookupType :: Ident -> T (Maybe Type)
lookupType ident = gets $ H.lookup ident

setType :: Ident -> Type -> T ()
setType ident typ = modify $ H.insert ident typ

throw :: TypeError -> T a
throw = throwError

runTWithBound :: Bindings -> T a -> Either TypeError a
runTWithBound bindings t = runExcept $ evalStateT t bindings

runT :: T a -> Either TypeError a
runT = runTWithBound H.empty
