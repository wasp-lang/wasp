module Generator.ServerGenerator
    ( genServer
    , operationsRouteInRootRouter
    ) where

import           Data.Aeson                                      (object, (.=))
import qualified Path                                            as P

import           CompileOptions                                  (CompileOptions)
import           Generator.Common                                (nodeVersionAsText)
import           Generator.ExternalCodeGenerator                 (generateExternalCodeDir)
import           Generator.FileDraft                             (FileDraft)
import           Generator.ServerGenerator.Common                (asServerFile,
                                                                  asTmplFile)
import qualified Generator.ServerGenerator.Common                as C
import qualified Generator.ServerGenerator.ExternalCodeGenerator as ServerExternalCodeGenerator
import           Generator.ServerGenerator.OperationsGenerator   (genOperations)
import           StrongPath                                      (File, Path,
                                                                  Rel)
import qualified StrongPath                                      as SP
import           Wasp                                            (Wasp)


genServer :: Wasp -> CompileOptions -> [FileDraft]
genServer wasp _ = concat
    [ [genReadme wasp]
    , [genPackageJson wasp]
    , [genNpmrc wasp]
    , [genNvmrc wasp]
    , [genGitignore wasp]
    , genSrcDir wasp
    , generateExternalCodeDir ServerExternalCodeGenerator.generatorStrategy wasp
    ]

genReadme :: Wasp -> FileDraft
genReadme _ = C.copyTmplAsIs (asTmplFile [P.relfile|README.md|])

genPackageJson :: Wasp -> FileDraft
genPackageJson _ = C.makeTemplateFD (asTmplFile [P.relfile|package.json|])
                              (asServerFile [P.relfile|package.json|])
                              (Just (object ["nodeVersion" .= nodeVersionAsText]))

genNpmrc :: Wasp -> FileDraft
genNpmrc _ = C.makeTemplateFD (asTmplFile [P.relfile|npmrc|])
                              (asServerFile [P.relfile|.npmrc|])
                              Nothing

genNvmrc :: Wasp -> FileDraft
genNvmrc _ = C.makeTemplateFD (asTmplFile [P.relfile|nvmrc|])
                              (asServerFile [P.relfile|.nvmrc|])
                              (Just (object ["nodeVersion" .= ('v' : nodeVersionAsText)]))

genGitignore :: Wasp -> FileDraft
genGitignore _ = C.makeTemplateFD (asTmplFile [P.relfile|gitignore|])
                                  (asServerFile [P.relfile|.gitignore|])
                                  Nothing

asTmplSrcFile :: P.Path P.Rel P.File -> Path (Rel C.ServerTemplatesSrcDir) File
asTmplSrcFile = SP.fromPathRelFile

genSrcDir :: Wasp -> [FileDraft]
genSrcDir wasp = concat
    [ [C.copySrcTmplAsIs $ asTmplSrcFile [P.relfile|app.js|]]
    , [C.copySrcTmplAsIs $ asTmplSrcFile [P.relfile|server.js|]]
    , [C.copySrcTmplAsIs $ asTmplSrcFile [P.relfile|utils.js|]]
    , [C.copySrcTmplAsIs $ asTmplSrcFile [P.relfile|core/HttpError.js|]]
    , genRoutesDir wasp
    , genOperations wasp
    ]

genRoutesDir :: Wasp -> [FileDraft]
genRoutesDir _ =
    -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
    -- but I did not bother with it yet since it is used only here for now.
    [ C.makeTemplateFD
        (asTmplFile [P.relfile|src/routes/index.js|])
        (asServerFile [P.relfile|src/routes/index.js|])
        (Just $ object [ "operationsRouteInRootRouter" .= operationsRouteInRootRouter ])
    ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "queries"
