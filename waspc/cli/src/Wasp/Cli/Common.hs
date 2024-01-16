module Wasp.Cli.Common
  ( WaspProjectDir,
    DotWaspDir,
    CliTemplatesDir,
    dotWaspDirInWaspProjectDir,
    dotWaspRootFileInWaspProjectDir,
    dotWaspInfoFileInGeneratedCodeDir,
    nodeModulesDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    buildDirInDotWaspDir,
    waspSays,
    waspWarns,
    waspScreams,
  )
where

import StrongPath (Dir, File', Path', Rel, reldir, relfile)
import qualified Wasp.Generator.Common
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Util.Terminal as Term

data DotWaspDir -- Here we put everything that wasp generates.

data NodeModulesDir

data CliTemplatesDir

-- TODO: SHould this be renamed to include word "root"?
dotWaspDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DotWaspDir)
dotWaspDirInWaspProjectDir = [reldir|.wasp|]

nodeModulesDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir NodeModulesDir)
nodeModulesDirInWaspProjectDir = [reldir|node_modules|]

-- TODO: Hm this has different name than it has in Generator.
generatedCodeDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Wasp.Generator.Common.ProjectRootDir)
generatedCodeDirInDotWaspDir = [reldir|out|]

buildDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Wasp.Generator.Common.ProjectRootDir)
buildDirInDotWaspDir = [reldir|build|]

dotWaspRootFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
dotWaspRootFileInWaspProjectDir = [relfile|.wasproot|]

dotWaspInfoFileInGeneratedCodeDir :: Path' (Rel Wasp.Generator.Common.ProjectRootDir) File'
dotWaspInfoFileInGeneratedCodeDir = [relfile|.waspinfo|]

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ Term.applyStyles [Term.Red] what
