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

    , module Wasp.Entity
    , getEntities
    , addEntity

    , module Wasp.EntityForm
    , getEntityFormsForEntity

    , module Wasp.Page
    , getPages
    , addPage
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import Wasp.App
import Wasp.Entity
import Wasp.EntityForm
import Wasp.JsImport
import Wasp.Page


-- * Wasp

data Wasp = Wasp
    { waspElements :: [WaspElement]
    , waspJsImports :: [JsImport]
    } deriving (Show, Eq)

data WaspElement
    = WaspElementApp !App
    | WaspElementPage !Page
    | WaspElementEntity !Entity
    | WaspElementEntityForm !EntityForm
    deriving (Show, Eq)

fromWaspElems :: [WaspElement] -> Wasp
fromWaspElems elems = Wasp { waspElements = elems, waspJsImports = [] }

-- * Js imports

getJsImports :: Wasp -> [JsImport]
getJsImports wasp = waspJsImports wasp

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
isAppElem _ = False

getApps :: Wasp -> [App]
getApps wasp = [app | (WaspElementApp app) <- waspElements wasp]

setApp :: Wasp -> App -> Wasp
setApp wasp app = wasp { waspElements = (WaspElementApp app) : (filter (not . isAppElem) (waspElements wasp)) }

fromApp :: App -> Wasp
fromApp app = fromWaspElems [WaspElementApp app]

-- * Pages

getPages :: Wasp -> [Page]
getPages wasp = [page | (WaspElementPage page) <- waspElements wasp]

addPage :: Wasp -> Page -> Wasp
addPage wasp page = wasp { waspElements = (WaspElementPage page):(waspElements wasp) }

-- * Entities

getEntities :: Wasp -> [Entity]
getEntities wasp = [entity | (WaspElementEntity entity) <- (waspElements wasp)]

addEntity :: Wasp -> Entity -> Wasp
addEntity wasp entity = wasp { waspElements = (WaspElementEntity entity):(waspElements wasp) }

-- * EntityForm

-- | Retrieves all entity forms for a given entity from a Wasp record.
getEntityFormsForEntity :: Wasp -> Entity -> [EntityForm]
getEntityFormsForEntity wasp entity = filter isFormOfGivenEntity allEntityForms
    where
        allEntityForms = [entityForm | (WaspElementEntityForm entityForm) <- waspElements wasp]
        isFormOfGivenEntity ef = entityName entity == efEntityName ef

-- * ToJSON instances.

instance ToJSON Wasp where
    toJSON wasp = object
        [ "app" .= getApp wasp
        , "pages" .= getPages wasp
        , "jsImports" .= getJsImports wasp
        ]
