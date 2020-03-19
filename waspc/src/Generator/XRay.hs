module Generator.XRay
    ( maybeGenerateXRayDir
    ) where

import Path ((</>), reldir, relfile)
import qualified Path.Aliases as Path

import Generator.FileDraft

maybeGenerateXRayDir :: Bool -> [FileDraft]
maybeGenerateXRayDir isXRayModeEnabled =
    if isXRayModeEnabled then generateXRayDir else []

generateXRayDir :: [FileDraft]
generateXRayDir = map (simpleNoDataTemplateFileDraft . (pathToXRayDir </>))
    [ [relfile|actions.js|] 
    , [relfile|actionTypes.js|]
    , [relfile|state.js|]
    , [relfile|InfoBox.js|]
    , [relfile|InfoBox.css|]
    , [relfile|EntityFormInfoBox.js|]
    , [relfile|EntityListInfoBox.js|]
    , [relfile|ButtonInfoBox.js|]
    , [relfile|JsonBrowser.js|]
    ]
    where
        pathToXRayDir = [reldir|src|] </> [reldir|xRay|]

simpleNoDataTemplateFileDraft :: Path.RelFile -> FileDraft
simpleNoDataTemplateFileDraft path = createTemplateFileDraft path path Nothing
