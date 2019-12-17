module Wasp
    ( Wasp
    , WaspElement (..)
    , fromWaspElems

    , JsImport (..)
    , getJsImports
    , setJsImports

    , App (..)
    , fromApp
    , getApp
    , setApp

    , Page (..)
    , getPages
    , addPage

    , Entity (..)
    , EntityField (..)
    , EntityFieldType (..)
    , getEntities
    , addEntity

    , EntityForm (..)
    , EntityFormSubmitConfig (..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text


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

-- * WaspJsImport

-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { jsImportWhat :: !String
    -- | Path of file to import, relative to external code directory.
    --   So for example if jsImportFrom is "test.js", we expect file
    --   to exist at <external_code_dir>/test.js.
    --   TODO: Make this more explicit in the code (both here and in wasp lang)? Also, support importing npm packages?
    , jsImportFrom :: !String
    } deriving (Show, Eq)

getJsImports :: Wasp -> [JsImport]
getJsImports wasp = waspJsImports wasp

setJsImports :: Wasp -> [JsImport] -> Wasp
setJsImports wasp jsImports = wasp { waspJsImports = jsImports }

-- * App

data App = App
    { appName :: !String -- Identifier
    , appTitle :: !String
    } deriving (Show, Eq)

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

-- * Page

data Page = Page
    { pageName :: !String
    , pageRoute :: !String
    , pageContent :: !String
    -- | TODO(martin): I did not know how to apply strictness annotation (!) here.
    , pageStyle :: Maybe Text
    } deriving (Show, Eq)

getPages :: Wasp -> [Page]
getPages wasp = [page | (WaspElementPage page) <- waspElements wasp]

addPage :: Wasp -> Page -> Wasp
addPage wasp page = wasp { waspElements = (WaspElementPage page):(waspElements wasp) }

-- * Entity

data Entity = Entity
    { entityName :: !String
    , entityFields :: ![EntityField]
    } deriving (Show, Eq)

data EntityField = EntityField
    { entityFieldName :: !String
    , entityFieldType :: !EntityFieldType
    } deriving (Show, Eq)

data EntityFieldType = EftString | EftBoolean deriving (Eq)

instance Show EntityFieldType where
    show EftString = "string"
    show EftBoolean = "boolean"

getEntities :: Wasp -> [Entity]
getEntities wasp = [entity | (WaspElementEntity entity) <- (waspElements wasp)]

addEntity :: Wasp -> Entity -> Wasp
addEntity wasp entity = wasp { waspElements = (WaspElementEntity entity):(waspElements wasp) }

-- * EntityForm

data EntityForm = EntityForm
    { efName :: !String -- Name of the form
    , efEntityName :: !String -- Name of the entity the form is linked to
    , efSubmitConfig :: Maybe EntityFormSubmitConfig
    } deriving (Show, Eq)

data EntityFormSubmitConfig = EntityFormSubmitConfig
    { onEnter :: !Bool
    } deriving (Show, Eq)

-- * ToJSON instances.

-- NOTE(martin): Here I define general transformation of App into JSON that I can then easily use
--   as data for templates, but we will probably want to replace this in the future with the better tailored
--   types that are exact fit for what is neeed, for example one type per template, which
--   will also enable us to check via types if template data is correctly shaped.
instance ToJSON App where
    toJSON app = object
        [ "name" .= appName app
        , "title" .= appTitle app
        ]

instance ToJSON Page where
    toJSON page = object
        [ "name" .= pageName page
        , "route" .= pageRoute page
        , "content" .= pageContent page
        ]

instance ToJSON Entity where
    toJSON entity = object
        [ "name" .= entityName entity
        , "fields" .= entityFields entity
        ]

instance ToJSON EntityField where
    toJSON entityField = object
        [ "name" .= entityFieldName entityField
        , "type" .= entityFieldType entityField
        ]

instance ToJSON EntityFieldType where
    toJSON = Aeson.String . Text.pack . show

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "what" .= jsImportWhat jsImport
        , "from" .= jsImportFrom jsImport
        ]

instance ToJSON Wasp where
    toJSON wasp = object
        [ "app" .= getApp wasp
        , "pages" .= getPages wasp
        , "jsImports" .= getJsImports wasp
        ]
