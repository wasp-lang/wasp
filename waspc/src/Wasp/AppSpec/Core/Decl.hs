{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Action (Action)
import Wasp.AppSpec.Api (Api)
import Wasp.AppSpec.ApiNamespace (ApiNamespace)
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Crud (Crud)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.Job (Job)
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Query (Query)
import Wasp.AppSpec.Route (Route)

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => String -> a -> Decl

instance Show Decl where
  show decl =
    show $
      (show <$> fromDecl @Api decl)
        <|> (show <$> fromDecl @Route decl)
        <|> (show <$> fromDecl @Crud decl)
        <|> (show <$> fromDecl @App decl)
        <|> (show <$> fromDecl @Action decl)
        <|> (show <$> fromDecl @Job decl)
        <|> (show <$> fromDecl @Entity decl)
        <|> (show <$> fromDecl @Page decl)
        <|> (show <$> fromDecl @ApiNamespace decl)
        <|> (show <$> fromDecl @Query decl)

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value
