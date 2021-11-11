module Wasp.Lib
  ( hello,
  )
where

import Wasp.Wasp (world)
import Wasp.Wasp.Hi (hi)

hello :: String
hello = hi ++ " " ++ world ++ "!"
