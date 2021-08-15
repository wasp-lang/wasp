module Cli.Common
  ( WaspProjectDir,
    DotWaspDir,
    CliTemplatesDir,
    dotWaspDirInWaspProjectDir,
    dotWaspRootFileInWaspProjectDir,
    dotWaspInfoFileInOutDir,
    extCodeDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    buildDirInDotWaspDir,
    waspSays,
  )
where

import Common (WaspProjectDir)
import ExternalCode (SourceExternalCodeDir)
import qualified Generator.Common
import StrongPath (Dir, File', Path', Rel, reldir, relfile)
import qualified Util.Terminal as Term

data DotWaspDir -- Here we put everything that wasp generates.

data CliTemplatesDir

-- TODO: SHould this be renamed to include word "root"?
dotWaspDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DotWaspDir)
dotWaspDirInWaspProjectDir = [reldir|.wasp|]

-- TODO: Hm this has different name than it has in Generator.
generatedCodeDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Generator.Common.ProjectRootDir)
generatedCodeDirInDotWaspDir = [reldir|out|]

buildDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Generator.Common.ProjectRootDir)
buildDirInDotWaspDir = [reldir|build|]

dotWaspRootFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
dotWaspRootFileInWaspProjectDir = [relfile|.wasproot|]

dotWaspInfoFileInOutDir :: Path' (Rel Generator.Common.ProjectRootDir) File'
dotWaspInfoFileInOutDir = [relfile|.waspinfo|]

extCodeDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
extCodeDirInWaspProjectDir = [reldir|ext|]

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what
