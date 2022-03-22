module Wasp.Analyzer.ErrorMessage
  ( makeFullErrorMsg,
  )
where

import Data.List (intercalate)
import Wasp.Util (indent)

makeFullErrorMsg :: String -> [String] -> String
makeFullErrorMsg errorMsg errorCtxMsgs
  | null errorCtxMsgs = errorMsg
  | otherwise = intercalate "\n\n" [errorMsg, concatErrorCtxMsgs errorCtxMsgs]

concatErrorCtxMsgs :: [String] -> String
concatErrorCtxMsgs [] = ""
concatErrorCtxMsgs msgChain = prefix ++ foldr1 appendMsg msgChain
  where
    prefix = "-> "
    appendMsg currMsg = (++) (currMsg ++ ":\n") . indent 2 . (prefix ++)