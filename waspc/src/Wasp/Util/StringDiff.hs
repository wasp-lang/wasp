module Wasp.Util.StringDiff where

data DiffType = Same | Different

printTwoFiles :: String -> String -> String
printTwoFiles text1 text2 = "===   First   ===\n" ++ text1 ++ "\n===   Second   ===\n" ++ text2 ++ "\n===   End   ===\n"
