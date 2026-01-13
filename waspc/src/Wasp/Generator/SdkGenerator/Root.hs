module Wasp.Generator.SdkGenerator.Root
  ( genRoot,
  )
where

import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.SdkGenerator.Root.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )

genRoot :: AppSpec -> N.NpmDepsForPackage -> Generator [FileDraft]
genRoot _spec npmDeps =
  sequence
    [ return $ mkTmplFd [relfile|tsconfig.json|],
      return $ mkTmplFd [relfile|tsconfig.sdk.json|],
      return $ mkTmplFd [relfile|copy-assets.js|],
      genPackageJson npmDeps
    ]

genPackageJson :: N.NpmDepsForPackage -> Generator FileDraft
genPackageJson npmDeps =
  return $ mkTmplFdWithData [relfile|package.json|] tmplData
  where
    tmplData =
      object
        [ "depsChunk" .= N.getDependenciesPackageJsonEntry npmDeps,
          "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry npmDeps,
          "peerDepsChunk" .= N.getPeerDependenciesPackageJsonEntry npmDeps
        ]
