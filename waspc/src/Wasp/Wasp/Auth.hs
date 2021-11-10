module Wasp.Auth
  ( Auth (..),
    AuthMethod (..),
  )
where

data Auth = Auth
  { _userEntity :: !String,
    _methods :: [AuthMethod],
    _onAuthFailedRedirectTo :: !String
  }
  deriving (Show, Eq)

data AuthMethod
  = EmailAndPassword
  deriving (Show, Eq)
