module Command.Deps
    ( deps,
    )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)

deps :: Command ()
deps = liftIO $ putStrLn "Hello"
