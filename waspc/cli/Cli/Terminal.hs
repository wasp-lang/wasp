module Cli.Terminal
  ( title,
  )
where

import qualified Util.Terminal as Term

title :: String -> String
title = Term.applyStyles [Term.Bold]
