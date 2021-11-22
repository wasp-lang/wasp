module Wasp.Wasp.Auth
  ( Auth (..),
    AuthMethod (..),
  )
where

data Auth = Auth
  { _userEntity :: !String,
    _methods :: [AuthMethod],
    _onAuthFailedRedirectTo :: !String,
    _onAuthSucceededRedirectTo :: !String
  }
  deriving (Show, Eq)

data AuthMethod
  = EmailAndPassword
  deriving (Show, Eq)
