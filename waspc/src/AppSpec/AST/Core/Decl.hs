{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module AppSpec.AST.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
  )
where

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

-- | Used to store a heterogenous lists of Wasp declarations, which make the top level
--   of AppSpec AST.
data Decl where
  Decl :: (Typeable a) => String -> a -> Decl

-- TODO: Before, all of these functions below had restriction that `a` has to be
--   instance of IsDeclType. Now we don't have that anymore.
--   Which means somebody could in theory try to get non-decl types with this,
--   and it doesn't seem to me like there is any easy way actually to figure out
--   which types (App, Page, ...) are actually DeclTypes, if we let their instances
--   be defined in Analyzer. Is this indicating that their instances should instead
--   be defined here? Shoudl we return the IsDeclType restriction here?
--   Also, they were separated into Decl.Operations -> we might want to do that again
--   if it will be needed.
--
--   Another approach I can imagine is that instead of having only IsDeclType class in
--   the Analyzer, we also introduce new IsDecl class here in AppSpec, and we use that
--   to restrict operations below. Then in Analyzer, we say that for some type to be
--   instance of IsDeclType, it has to already be instance of IsDecl, and that should do it,
--   we have separation but still it is clear which types are declarations and you can't
--   use the wrong one here. Actually this sounds pretty good.
--   On thing here is that naming seems a bit off -> what is the difference between IsDecl
--   and IsDeclType? Maybe we should instead go with AppSpec.IsDeclType and Analyzer.IsDeclType?
--   Maybe the fact that we can't figure out different names indicates these should not
--   be separate things? Not sure, I have to think more about this.

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (Typeable a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (Typeable a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (Typeable a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value
