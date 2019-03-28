module Generator (
  generateWebApp
) where

import System.FilePath (FilePath, (</>))

import Wasp

generateWebApp :: Wasp -> FilePath -> IO ()
generateWebApp wasp outDir = writeFiles outDir
    [ generateReadme wasp
    , generatePackageJson wasp
    , generateGitignore wasp
    , generatePublicDir wasp
    , generateSrcDir wasp
    ]

-- TODO: We should parallelize this.
writeFiles :: FilePath -> [FileToWrite] -> IO ()
writeFiles rootDir files = sequence_ $ map write files
  where
    write file = writeFile (rootDir </> (relativePath file)) (content file)

data FileToWrite = FileToWrite
    { relativePath :: FilePath
    , content :: String
    }

-- TODO: I need to rewrite the generators to use: templates + mustache. I also need to make dirs work like dirs.
reactAppTemplatesPath = "../resources/Generator/templates/react-app"

type FileGenerator = Wasp -> FileToWrite

generateReadme :: FileGenerator
generateReadme wasp = FileToWrite "README.md" "TEST!"

generatePackageJson :: FileGenerator
generatePackageJson wasp = FileToWrite "package.json" "TEST!"

generateGitignore :: FileGenerator
generateGitignore wasp = FileToWrite ".gitignore" "TEST!"

generatePublicDir :: FileGenerator
generatePublicDir wasp = FileToWrite "public" "TEST!"

generateSrcDir :: FileGenerator
generateSrcDir wasp = FileToWrite "src" "TEST!"
