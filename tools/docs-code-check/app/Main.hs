module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- TODO: Make this path not hardcoded. Paths module?
docsDir :: FilePath
docsDir = "/home/martin/git/wasp-lang/waspc/web/docs/"

-- In all the files taht match provided extensions (.md, .mdx, .tsx), search for special comments.

specialCommentRegex :: String
specialCommentRegex = "^\\s*//\\s*CODEREF\\s+"
