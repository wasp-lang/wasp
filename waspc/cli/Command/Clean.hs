module Command.Clean
    ( clean
    ) where

import           Control.Monad.IO.Class (liftIO)
import           System.Directory       (doesDirectoryExist,
                                         removeDirectoryRecursive)
import           System.IO              (hFlush, stdout)

import qualified Cli.Common             as Common
import           Command                (Command)
import           Command.Common         (findWaspProjectRootDirFromCwd)
import qualified StrongPath             as SP

clean :: Command ()
clean = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let dotWaspDirFp = SP.toFilePath $ waspProjectDir SP.</> Common.dotWaspDirInWaspProjectDir
    liftIO $ putStrLn "Deleting .wasp/ directory..." >> hFlush stdout
    doesDotWaspDirExist <- liftIO $ doesDirectoryExist dotWaspDirFp
    if doesDotWaspDirExist
        then liftIO $ do removeDirectoryRecursive dotWaspDirFp
                         putStrLn "Deleted .wasp/ directory."
        else liftIO $ putStrLn "Nothing to delete: .wasp directory does not exist."

