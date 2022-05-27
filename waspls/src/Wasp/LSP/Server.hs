module Wasp.LSP.Server
  ( run,
  )
where

run :: Maybe FilePath -> IO ()
run logFile = putStrLn $ "Hello from waspls (logging to " ++ show logFile ++ ")"
