{-# LANGUAGE OverloadedStrings #-}
module Wasp
    ( Wasp
    , App (..)
    , fromApp
    , getApp
    , setApp
    ) where

import qualified Data.Aeson as Aeson


data Wasp = Wasp [WaspElement] deriving (Show, Eq)

data WaspElement
    = WaspElementApp !App
    | WaspElementPage
    | WaspElementEntity
    deriving (Show, Eq)

data App = App
    { appName :: !String -- Identifier
    , appTitle :: !String -- Title
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
getApps (Wasp elems) = map getAppFromElem $ filter isAppElem elems
  where
    getAppFromElem (WaspElementApp app) = app
    getAppFromElem _ = error "Not an app"

setApp :: Wasp -> App -> Wasp
setApp (Wasp elems) app = Wasp $ (WaspElementApp app) : (filter (not . isAppElem) elems)

fromApp :: App -> Wasp
fromApp app = Wasp [WaspElementApp app]


-- NOTE(martin): Here I define general transformation of App into JSON that I can then easily use
--   as data for templates, but we will probably want to replace this in the future with the better tailored
--   types that are exact fit for what is neeed (for example one type per template).
instance Aeson.ToJSON App where
    toJSON app = Aeson.object
        [ "name" Aeson..= appName app
        , "title" Aeson..= appTitle app
        ]
instance Aeson.ToJSON Wasp where
    toJSON wasp = Aeson.object
        [ "app" Aeson..= getApp wasp
        ]
