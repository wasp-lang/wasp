module Generator.WebAppGenerator.ButtonGenerator
       ( generateButtons
       ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified System.FilePath as FP
import qualified Path as P

import Path.Extra (reversePath)
import StrongPath (Path, Rel, Dir, File, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Button as WButton
import Generator.FileDraft (FileDraft)
import qualified Generator.WebAppGenerator.EntityGenerator as EntityGen
import qualified Generator.WebAppGenerator.Common as Common

generateButtons :: Wasp -> [FileDraft]
generateButtons wasp = concatMap (generateButton wasp) (Wasp.getButtons wasp)

generateButton :: Wasp -> WButton.Button -> [FileDraft]
generateButton wasp button =
    [ generateButtonComponent wasp button
    ]

generateButtonComponent :: Wasp -> WButton.Button -> FileDraft
generateButtonComponent wasp button = Common.makeTemplateFD tmplPath dstPath (Just templateData)
  where
    tmplPath :: Path (Rel Common.WebAppTemplatesDir) File
    tmplPath = (SP.fromPathRelFile [P.relfile|src/components/_Button.js|])

    dstPath = Common.webAppSrcDirInWebAppRootDir
              </> buttonDirPathInSrc
              </> (fromJust $ SP.parseRelFile $ (WButton._name button) ++ ".js")

    onClickActionData :: Maybe Aeson.Value
    onClickActionData = do
      actionName <- WButton._onClickActionName button
      action <- Wasp.getActionByName wasp actionName
      let (pathInSrc, exportedIdentifier) = EntityGen.getImportInfoForAction wasp action
      return $ object [ "importPath" .= buildImportPathFromPathInSrc pathInSrc
                      , "exportedIdentifier" .= exportedIdentifier
                      ]

    templateData = object $
        [ "wasp" .= wasp
        , "button" .= button
        ]
        ++ maybe [] (\d -> ["onClickAction" .= d]) onClickActionData

data ButtonDir

buttonDirPathInSrc :: Path (Rel Common.WebAppSrcDir) (Dir ButtonDir)
buttonDirPathInSrc = SP.fromPathRelDir [P.reldir|components|]

-- | Takes path relative to the src path of generated project and turns it into relative path that can be
-- used as "from" part of the import in the button component source file.
-- NOTE: Here we return FilePath instead of Path because we need stuff like "./" or "../" in the path,
-- which Path would normalize away.
buildImportPathFromPathInSrc :: Path (Rel Common.WebAppSrcDir) a -> FilePath
buildImportPathFromPathInSrc pathInSrc = (reversePath $ SP.toPathRelDir buttonDirPathInSrc)
                                         FP.</> (SP.toFilePath pathInSrc)
