module Generator.Button
       ( generateButtons
       ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified System.FilePath as FP
import Path ((</>), relfile, reldir)
import qualified Path
import qualified Path.Aliases as Path
import qualified Path.Extra as Path

import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Button as WButton
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import qualified Generator.Entity
import qualified Generator.Common as Common

generateButtons :: Wasp -> [FileDraft]
generateButtons wasp = concatMap (generateButton wasp) (Wasp.getButtons wasp)

generateButton :: Wasp -> WButton.Button -> [FileDraft]
generateButton wasp button =
    [ generateButtonComponent wasp button
    ]

generateButtonComponent :: Wasp -> WButton.Button -> FileDraft
generateButtonComponent wasp button = createTemplateFileDraft dstPath srcPath (Just templateData)
  where
    srcPath = [reldir|src|] </> [reldir|components|] </> [relfile|_Button.js|]
    dstPath = Common.srcDirPath </> buttonDirPathInSrc </> (fromJust $ Path.parseRelFile $ (WButton._name button) ++ ".js")

    onClickActionData :: Maybe Aeson.Value
    onClickActionData = do
      actionName <- WButton._onClickActionName button
      action <- Wasp.getActionByName wasp actionName
      let (pathInSrc, exportedIdentifier) = Generator.Entity.getImportInfoForAction wasp action
      return $ object [ "importPath" .= buildImportPathFromPathInSrc pathInSrc
                      , "exportedIdentifier" .= exportedIdentifier
                      ]

    templateData = object $
        [ "wasp" .= wasp
        , "button" .= button
        ]
        ++ maybe [] (\d -> ["onClickAction" .= d]) onClickActionData

buttonDirPathInSrc :: Path.RelDir
buttonDirPathInSrc = [reldir|components|]

-- | Takes path relative to the src path of generated project and turns it into relative path that can be
-- used as "from" part of the import in the button component source file.
-- NOTE: Here we return FilePath instead of Path because we need stuff like "./" or "../" in the path,
-- which Path would normalize away.
buildImportPathFromPathInSrc :: Path.Path Path.Rel a -> FilePath
buildImportPathFromPathInSrc pathInSrc = (Path.reversePath buttonDirPathInSrc) FP.</> (Path.toFilePath pathInSrc)
