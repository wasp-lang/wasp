module Wasp.Generator.SdkGenerator.Client.AppGenerator
  ( genClientApp,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.WebSocket as AS.WS

genClientApp :: AppSpec -> Generator [FileDraft]
genClientApp spec =
  sequence
    [ genAppWaspApp spec,
      genAppIndex
    ]

genAppIndex :: Generator FileDraft
genAppIndex = return $ C.mkTmplFd [relfile|client/app/index.ts|]

genAppWaspApp :: AppSpec -> Generator FileDraft
genAppWaspApp spec = return $ C.mkTmplFdWithData [relfile|client/app/WaspApp.tsx|] tmplData
  where
    tmplData =
      object
        [ "setupFn" .= extImportToImportJson maybeSetupJsFunction,
          "areWebSocketsUsed" .= AS.WS.areWebSocketsUsed spec
        ]

    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
