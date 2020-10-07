module Wasp
    ( Wasp
    , WaspElement (..)
    , fromWaspElems

    , module Wasp.JsImport
    , getJsImports
    , setJsImports

    , module Wasp.App
    , fromApp
    , getApp
    , setApp

    , getPSLEntities

    -- TODO(matija): Old Entity stuff, to be removed.
    , module Wasp.Entity
    , getEntities
    , addEntity
    , getEntityByName

    , getEntityFormsForEntity
    , getEntityListsForEntity

    , module Wasp.Page
    , getPages
    , addPage
    , getRoutes

    , getQueries
    , addQuery
    , getQueryByName

    , getActions
    , addAction
    , getActionByName

    , setExternalCodeFiles
    , getExternalCodeFiles

    , setNpmDependencies
    , getNpmDependencies
    ) where

import           Data.Aeson           (ToJSON (..), object, (.=))

import qualified ExternalCode
import qualified Util                 as U
import qualified Wasp.Action
import           Wasp.App
import qualified Wasp.Auth
import           Wasp.EntityPSL
import           Wasp.JsImport
import           Wasp.NpmDependencies (NpmDependencies)
import qualified Wasp.NpmDependencies
import           Wasp.Page
import qualified Wasp.Query
import           Wasp.Route

-- TODO(matija): old Entity stuff, to be removed
import           Wasp.Entity
import qualified Wasp.EntityForm      as EF
import qualified Wasp.EntityList      as EL


-- * Wasp

data Wasp = Wasp
    { waspElements      :: [WaspElement]
    , waspJsImports     :: [JsImport]
    , externalCodeFiles :: [ExternalCode.File]
    } deriving (Show, Eq)

data WaspElement
    = WaspElementApp !App
    | WaspElementAuth !Wasp.Auth.Auth
    | WaspElementPage !Page
    | WaspElementNpmDependencies !NpmDependencies
    | WaspElementRoute !Route
    | WaspElementEntityPSL !Wasp.EntityPSL.EntityPSL
    | WaspElementQuery !Wasp.Query.Query
    | WaspElementAction !Wasp.Action.Action

    -- TODO(matija): old Entity stuff, to be removed
    | WaspElementEntity !Entity
    | WaspElementEntityForm !EF.EntityForm
    | WaspElementEntityList !EL.EntityList

    deriving (Show, Eq)

fromWaspElems :: [WaspElement] -> Wasp
fromWaspElems elems = Wasp
    { waspElements = elems
    , waspJsImports = []
    , externalCodeFiles = []
    }

-- * External code files

getExternalCodeFiles :: Wasp -> [ExternalCode.File]
getExternalCodeFiles = externalCodeFiles

setExternalCodeFiles :: Wasp -> [ExternalCode.File] -> Wasp
setExternalCodeFiles wasp files = wasp { externalCodeFiles = files }

-- * Js imports

getJsImports :: Wasp -> [JsImport]
getJsImports = waspJsImports

setJsImports :: Wasp -> [JsImport] -> Wasp
setJsImports wasp jsImports = wasp { waspJsImports = jsImports }

-- * App

getApp :: Wasp -> App
getApp wasp = let apps = getApps wasp in
    if (length apps /= 1)
    then error "Wasp has to contain exactly one WaspElementApp element!"
    else head apps

isAppElem :: WaspElement -> Bool
isAppElem WaspElementApp{} = True
isAppElem _                = False

getApps :: Wasp -> [App]
getApps wasp = [app | (WaspElementApp app) <- waspElements wasp]

setApp :: Wasp -> App -> Wasp
setApp wasp app = wasp { waspElements = (WaspElementApp app) : (filter (not . isAppElem) (waspElements wasp)) }

fromApp :: App -> Wasp
fromApp app = fromWaspElems [WaspElementApp app]

-- * NpmDependencies

getNpmDependencies :: Wasp -> NpmDependencies
getNpmDependencies wasp
    = let depses = [d | (WaspElementNpmDependencies d) <- waspElements wasp]
      in case depses of
         []     -> Wasp.NpmDependencies.empty
         [deps] -> deps
         _      -> error "Wasp can't contain more than one NpmDependencies element!"

isNpmDependenciesElem :: WaspElement -> Bool
isNpmDependenciesElem WaspElementNpmDependencies{} = True
isNpmDependenciesElem _                            = False

setNpmDependencies :: Wasp -> NpmDependencies -> Wasp
setNpmDependencies wasp deps = wasp
    { waspElements = WaspElementNpmDependencies deps : filter (not . isNpmDependenciesElem) (waspElements wasp)
    }

-- * Routes

getRoutes :: Wasp -> [Route]
getRoutes wasp = [route | (WaspElementRoute route) <- waspElements wasp]

-- * Pages

getPages :: Wasp -> [Page]
getPages wasp = [page | (WaspElementPage page) <- waspElements wasp]

addPage :: Wasp -> Page -> Wasp
addPage wasp page = wasp { waspElements = (WaspElementPage page):(waspElements wasp) }

-- * Query

getQueries :: Wasp -> [Wasp.Query.Query]
getQueries wasp = [query | (WaspElementQuery query) <- waspElements wasp]

addQuery :: Wasp -> Wasp.Query.Query -> Wasp
addQuery wasp query = wasp { waspElements = WaspElementQuery query : waspElements wasp }

-- | Gets query with a specified name from wasp, if such an action exists.
-- We assume here that there are no two queries with same name.
getQueryByName :: Wasp -> String -> Maybe Wasp.Query.Query
getQueryByName wasp name = U.headSafe $ filter (\a -> Wasp.Query._name a == name) (getQueries wasp)

-- * Action

getActions :: Wasp -> [Wasp.Action.Action]
getActions wasp = [action | (WaspElementAction action) <- waspElements wasp]

addAction :: Wasp -> Wasp.Action.Action -> Wasp
addAction wasp action = wasp { waspElements = WaspElementAction action : waspElements wasp }

-- | Gets action with a specified name from wasp, if such an action exists.
-- We assume here that there are no two actions with same name.
getActionByName :: Wasp -> String -> Maybe Wasp.Action.Action
getActionByName wasp name = U.headSafe $ filter (\a -> Wasp.Action._name a == name) (getActions wasp)

-- * Entities

getPSLEntities :: Wasp -> [Wasp.EntityPSL.EntityPSL]
getPSLEntities wasp = [entityPSL | (WaspElementEntityPSL entityPSL) <- (waspElements wasp)]


-- TODO(matija): old Entity stuff, to be removed
getEntities :: Wasp -> [Entity]
getEntities wasp = [entity | (WaspElementEntity entity) <- (waspElements wasp)]

-- | Gets entity with a specified name from wasp, if such an entity exists.
-- Entity name must be unique, so there can be no more than one such entity.
getEntityByName :: Wasp -> String -> Maybe Entity
getEntityByName wasp name = U.headSafe $ filter (\e -> entityName e == name) (getEntities wasp)

addEntity :: Wasp -> Entity -> Wasp
addEntity wasp entity = wasp { waspElements = (WaspElementEntity entity):(waspElements wasp) }

-- * EntityForm

-- | Retrieves all entity forms for a given entity from a Wasp record.
getEntityFormsForEntity :: Wasp -> Entity -> [EF.EntityForm]
getEntityFormsForEntity wasp entity = filter isFormOfGivenEntity allEntityForms
    where
        allEntityForms = [entityForm | (WaspElementEntityForm entityForm) <- waspElements wasp]
        isFormOfGivenEntity ef = entityName entity == EF._entityName ef

-- * EntityList

-- | Retrieves all entity lists for a given entity from a Wasp record.
getEntityListsForEntity :: Wasp -> Entity -> [EL.EntityList]
getEntityListsForEntity wasp entity = filter isListOfGivenEntity allEntityLists
    where
        allEntityLists = [entityList | (WaspElementEntityList entityList) <- waspElements wasp]
        isListOfGivenEntity el = entityName entity == EL._entityName el

-- * ToJSON instances.

instance ToJSON Wasp where
    toJSON wasp = object
        [ "app" .= getApp wasp
        , "pages" .= getPages wasp
        , "routes" .= getRoutes wasp
        , "jsImports" .= getJsImports wasp
        ]
