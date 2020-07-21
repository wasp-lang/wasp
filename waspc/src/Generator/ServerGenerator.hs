module Generator.ServerGenerator
    ( genServer
    ) where

import Path ((</>), relfile, reldir)
import qualified Path.Aliases as Path

import Wasp (Wasp)
import CompileOptions (CompileOptions)
import Generator.FileDraft (FileDraft)
import Generator.ServerGenerator.Common as C


genServer :: Wasp -> CompileOptions -> [FileDraft]
genServer wasp _ = concat
    [ [genReadme wasp]
    , [genPackageJson wasp]
    , [genNpmrc wasp]
    , [genNvmrc wasp]
    , [genGitignore wasp]
    , genSrcDir wasp
    ]

genReadme :: Wasp -> FileDraft
genReadme _ = C.copyTmplAsIs [relfile|README.md|]

genPackageJson :: Wasp -> FileDraft
genPackageJson _ = C.copyTmplAsIs [relfile|package.json|]

genNpmrc :: Wasp -> FileDraft
genNpmrc _ = C.makeTemplateFD [relfile|npmrc|] [relfile|.npmrc|] Nothing

genNvmrc :: Wasp -> FileDraft
genNvmrc _ = C.makeTemplateFD [relfile|nvmrc|] [relfile|.nvmrc|] Nothing

genGitignore :: Wasp -> FileDraft
genGitignore _ = C.makeTemplateFD [relfile|gitignore|] [relfile|.gitignore|] Nothing

genSrcDir :: Wasp -> [FileDraft]
genSrcDir wasp = concat
    [ [copySrcTmpl [relfile|app.js|]]
    , [copySrcTmpl [relfile|server.js|]]
    , genRoutesDir wasp
    ]

genRoutesDir :: Wasp -> [FileDraft]
genRoutesDir _ =
    -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
    -- but I did not bother with it yet since it is used only here for now.
    [ copySrcTmpl [relfile|routes/index.js|]
    ]

copySrcTmpl :: Path.RelFile -> FileDraft
copySrcTmpl pathInTemplatesSrcDir = C.makeTemplateFD srcPath dstPath Nothing
  where srcPath = [reldir|src|] </> pathInTemplatesSrcDir
        dstPath = C.serverSrcDirInServerRootDir </> pathInTemplatesSrcDir
