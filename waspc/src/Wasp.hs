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

    , getAuth
    , getPSLEntities

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

    , setDotEnvFile
    , getDotEnvFile

    , setIsBuild
    , getIsBuild

    , setNpmDependencies
    , getNpmDependencies
    ) where

import           Data.Aeson           (ToJSON (..), object, (.=))
import           StrongPath           (Path, Abs, File)

import qualified ExternalCode
import qualified Util                 as U
import qualified Wasp.Action
import           Wasp.App
import qualified Wasp.Auth
import qualified Wasp.Db
import           Wasp.Entity
import           Wasp.JsImport
import           Wasp.NpmDependencies (NpmDependencies)
import qualified Wasp.NpmDependencies
import           Wasp.Page
import qualified Wasp.Query
import           Wasp.Route


-- * Wasp

data Wasp = Wasp
    { waspElements      :: [WaspElement]
    , waspJsImports     :: [JsImport]
    , externalCodeFiles :: [ExternalCode.File]
    , dotEnvFile        :: Maybe (Path Abs File)
    , isBuild           :: Bool
    } deriving (Show, Eq)

data WaspElement
    = WaspElementApp !App
    | WaspElementAuth !Wasp.Auth.Auth
    | WaspElementDb !Wasp.Db.Db
    | WaspElementPage !Page
    | WaspElementNpmDependencies !NpmDependencies
    | WaspElementRoute !Route
    | WaspElementEntity !Wasp.Entity.Entity
    | WaspElementQuery !Wasp.Query.Query
    | WaspElementAction !Wasp.Action.Action
    deriving (Show, Eq)

fromWaspElems :: [WaspElement] -> Wasp
fromWaspElems elems = Wasp
    { waspElements = elems
    , waspJsImports = []
    , externalCodeFiles = []
    , dotEnvFile = Nothing
    , isBuild = False
    }

-- * Build

getIsBuild :: Wasp -> Bool
getIsBuild = isBuild

setIsBuild :: Wasp -> Bool -> Wasp
setIsBuild wasp isBuildNew = wasp { isBuild = isBuildNew }

-- * External code files

getExternalCodeFiles :: Wasp -> [ExternalCode.File]
getExternalCodeFiles = externalCodeFiles

setExternalCodeFiles :: Wasp -> [ExternalCode.File] -> Wasp
setExternalCodeFiles wasp files = wasp { externalCodeFiles = files }

-- * Dot env files

getDotEnvFile :: Wasp -> Maybe (Path Abs File)
getDotEnvFile = dotEnvFile

setDotEnvFile :: Wasp -> Maybe (Path Abs File) -> Wasp
setDotEnvFile wasp file = wasp { dotEnvFile = file }

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

-- * Auth

getAuth :: Wasp -> Maybe Wasp.Auth.Auth
getAuth wasp = let auths = [a | WaspElementAuth a <- waspElements wasp] in
    case auths of
        []  -> Nothing
        [a] -> Just a
        _   -> error "Wasp can't contain more than one WaspElementAuth element!"

-- * Db

getDb :: Wasp -> Maybe Wasp.Db.Db
getDb wasp = let dbs = [db | WaspElementDb db <- waspElements wasp] in
    case dbs of
        []  -> Nothing
        [db] -> Just db
        _   -> error "Wasp can't contain more than one Db element!"

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

getPSLEntities :: Wasp -> [Wasp.Entity.Entity]
getPSLEntities wasp = [entity | (WaspElementEntity entity) <- (waspElements wasp)]


-- * ToJSON instances.

instance ToJSON Wasp where
    toJSON wasp = object
        [ "app" .= getApp wasp
        , "pages" .= getPages wasp
        , "routes" .= getRoutes wasp
        , "jsImports" .= getJsImports wasp
        ]
