module Wasp.Generator.SdkGenerator.NodeCommands
  ( compileSdk,
    copySdkAssets,
  )
where

import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.Generator.SdkGenerator.Common (SdkRootDir)

compileSdk :: NodeCommand SdkRootDir
compileSdk = NodeCommand "tsc" []

copySdkAssets :: NodeCommand SdkRootDir
copySdkAssets = NodeCommand "node" ["./scripts/copy-assets.js"]
