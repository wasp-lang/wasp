module Wasp.Cli.Terminal
  ( title,
  )
where

import qualified Wasp.Util.Terminal as Term

title :: String -> String
title = Term.applyStyles [Term.Bold]
