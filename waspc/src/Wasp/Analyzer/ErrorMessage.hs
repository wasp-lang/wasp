module Wasp.Analyzer.ErrorMessage
  ( makeFullErrorMsg,
  )
where

import Wasp.Util (indent)

makeFullErrorMsg :: String -> [String] -> String
makeFullErrorMsg errorMsg errorCtxMsgs =
  errorMsg ++ (if null errorCtxMsgs then "" else concatErrorCtxMsgs errorCtxMsgs)

concatErrorCtxMsgs :: [String] -> String
concatErrorCtxMsgs [] = ""
concatErrorCtxMsgs msgChain = prefix ++ foldr1 appendMsg msgChain
  where
    prefix = "-> "
    appendMsg currMsg = (++) (currMsg ++ ":\n") . indent 2 . (prefix ++)
