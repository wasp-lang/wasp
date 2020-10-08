module Wasp.Auth
    ( Auth (..)
    , AuthMethod (..)
    ) where

data Auth = Auth
    { _userEntity :: !String
    , _methods :: [AuthMethod]
    } deriving (Show, Eq)

data AuthMethod
    = EmailAndPassword
    deriving (Show, Eq)
