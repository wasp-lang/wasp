module Wasp
    ( Wasp
    , App (..)
    , fromApp
    , getApp
    , setApp
    ) where

data Wasp = Wasp [WaspElement] deriving (Show)

data WaspElement
    = WaspElementApp !App
    | WaspElementPage
    | WaspElementEntity
    deriving (Show)

data App = App
    { appName :: !String -- Identifier
    , appTitle :: !String -- Title
    } deriving (Show)

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

setApp :: Wasp -> App -> Wasp
setApp (Wasp elems) app = Wasp $ (WaspElementApp app) : (filter (not . isAppElem) elems)

fromApp :: App -> Wasp
fromApp app = Wasp [WaspElementApp app]
