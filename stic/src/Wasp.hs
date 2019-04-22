{-# LANGUAGE OverloadedStrings #-}
module Wasp
    ( Wasp
    , WaspElement (..)
    , fromWaspElems

    , App (..)
    , fromApp
    , getApp
    , setApp

    , Page (..)
    , getPages
    , addPage
    ) where

import Data.Aeson ((.=), object, ToJSON(..))


-- * Wasp

data Wasp = Wasp [WaspElement] deriving (Show, Eq)

data WaspElement
    = WaspElementApp !App
    | WaspElementPage !Page
    | WaspElementEntity
    deriving (Show, Eq)

fromWaspElems :: [WaspElement] -> Wasp
fromWaspElems elems = Wasp elems


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
getApps (Wasp elems) = [app | (WaspElementApp app) <- elems]

setApp :: Wasp -> App -> Wasp
setApp (Wasp elems) app = Wasp $ (WaspElementApp app) : (filter (not . isAppElem) elems)

fromApp :: App -> Wasp
fromApp app = Wasp [WaspElementApp app]


-- * Page

data Page = Page
    { pageName :: !String  -- Identifier
    , pageRoute :: !String
    , pageContent :: !String
    } deriving (Show, Eq)

getPages :: Wasp -> [Page]
getPages (Wasp elems) = [page | (WaspElementPage page) <- elems]

addPage :: Wasp -> Page -> Wasp
addPage (Wasp elems) page = Wasp $ (WaspElementPage page):elems


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
instance ToJSON Wasp where
    toJSON wasp = object
        [ "app" .= getApp wasp
        , "pages" .= getPages wasp
        ]
