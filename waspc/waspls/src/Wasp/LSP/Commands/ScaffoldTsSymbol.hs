{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Wasp.LSP.Commands.ScaffoldTsSymbol
  ( -- * Scaffold TS Symbol Command

    -- Command @wasp.scaffold.ts-symbol@ appends a new function with the given
    -- name to end of a particular file.
    --
    -- Mustache templates are used to write the code for the new function.
    -- @templateForRequest@ chooses a template in @data/Lsp/template/ts@ based
    -- on the location of the external import and the extension of the file that
    -- the function will be appended to.
    --
    -- To add a new template, add the template to the template directory. Then,
    -- add a new equation to @templateForRequest@ that matches when you want the
    -- template to be used.
    --
    -- The templates have several variables available to them:
    --
    -- [@name@]: String, the name imported by a the external import. For example,
    --   @getAll@ in @import { getAll } from "@server/queries.js"@.
    --
    -- [@default?@]: Boolean, true if the external import is importing the default
    --   export.
    --
    -- [@named?@]: Boolean, true if the external import is importing a named export.
    --
    -- [@upperDeclName@]: String, the name of the declaration the external import
    --   is within, with the first letter capitalized.

    -- ** Current Limitations

    -- Due to the current way waspls works, we are not able to send a
    -- 'LSP.WorkspaceApplyEdit' request to add the scaffolded code. Instead, we
    -- modify the file on disk. This isn't the exact proper way to modify the
    -- code, but it works.
    --
    -- The reason is that waspls only receives document synchronization events
    -- for @.wasp@ files (didChange, didOpen, didClose, etc.), so we only know
    -- the versioned URI for the @.wasp@ file, not the TS files. But we need the
    -- versioned URI for sending edits. The LSP spec is not clear on what files
    -- get the sync events, but in VSCode's case it is determined by the client
    -- (the VSCode Wasp extension).
    --
    -- The extension can easily be configured to send sync events for JS/TS
    -- files, but waspls makes a lot of assumptions about what files it receives
    -- events for and would require a lot of refactoring to work properly. It
    -- would be best to handle this at the same time as we add support for multiple
    -- wasp files to the language and to waspls.
    Args (Args, symbolName, pathToExtImport, filepath),
    hasTemplateForArgs,
    command,
    makeLspCommand,
  )
where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (FromJSON, ToJSON (toJSON), object, parseJSON, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import qualified System.Directory as Dir
import qualified Text.Mustache as Mustache
import Text.Printf (printf)
import Wasp.Analyzer.Parser.AST (ExtImportName (ExtImportField, ExtImportModule))
import qualified Wasp.Data
import Wasp.LSP.Analysis (publishDiagnostics)
import Wasp.LSP.Commands.Command (Command (Command, commandHandler, commandName), makeInvalidParamsError, withParsedArgs)
import Wasp.LSP.ExtImport.Diagnostic (updateMissingExtImportDiagnostics)
import Wasp.LSP.ExtImport.ExportsCache (refreshExportsOfFiles)
import Wasp.LSP.ServerMonads (ServerM)
import qualified Wasp.LSP.ServerMonads as ServerM
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.TypeInference (ExprPath, ExprPathStep (Decl, DictKey))
import qualified Wasp.LSP.TypeInference as Inference
import Wasp.Util (ifM, toUpperFirst)
import Wasp.Util.IO (doesFileExist)

command :: Command
command =
  Command
    { commandName = "wasp.scaffold.ts-symbol",
      commandHandler = handler
    }

makeLspCommand :: Args -> LSP.Command
makeLspCommand args =
  LSP.Command
    { _title = "Scaffold TS Code",
      _command = commandName command,
      _arguments = Just $ LSP.List [toJSON args]
    }

data Args = Args
  { -- | Name of the symbol to define. If this is 'ExtImportModule', it will
    -- create a function with the specified name and export it as default.
    symbolName :: ExtImportName,
    -- | Description of where the external import occurs in the wasp source code,
    -- used, along with the extension of 'filepath', to determine what template
    -- to use.
    pathToExtImport :: ExprPath,
    -- | Path to the file to add the scaffolded code to the end of.
    filepath :: SP.Path' SP.Abs SP.File'
  }
  deriving (Show, Eq)

instance ToJSON Args where
  toJSON args =
    object
      [ "symbolName" .= symbolName args,
        "pathToExtImport" .= pathToExtImport args,
        "filepath" .= SP.toFilePath (filepath args)
      ]

instance FromJSON Args where
  parseJSON = withObject "Args" $ \v ->
    Args
      <$> v .: "symbolName"
      <*> v .: "pathToExtImport"
      <*> ((maybe (fail "Could not parse filepath") pure . SP.parseAbsFile) =<< v .: "filepath")

handler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
handler request respond = withParsedArgs request respond scaffold
  where
    scaffold :: Args -> ServerM ()
    scaffold args@Args {..} = case P.fileExtension $ SP.toPathAbsFile filepath of
      Nothing -> respond $ Left $ makeInvalidParamsError "Invalid filepath: no extension"
      Just ext ->
        getTemplateFor pathToExtImport ext >>= \case
          Left err ->
            respond $ Left $ makeInvalidParamsError $ Text.pack err
          Right template -> renderAndWriteScaffoldTemplate args template

    renderAndWriteScaffoldTemplate :: Args -> Mustache.Template -> ServerM ()
    renderAndWriteScaffoldTemplate args@Args {..} template = case pathToExtImport of
      Inference.Decl _ declName : _ -> do
        let symbolData = case symbolName of
              ExtImportModule name -> ["default?" .= True, "named?" .= False, "name" .= name]
              ExtImportField name -> ["default?" .= False, "named?" .= True, "name" .= name]
        let templateData = object $ symbolData ++ ["upperDeclName" .= toUpperFirst declName]

        let rendered = renderTemplate template templateData
        logM $ printf "[wasp.scaffold.ts-symbol]: rendered=%s" (show rendered)

        -- NOTE: we modify the file on disk instead of applying an edit through
        -- the LSP client. See "Current Limitations" above.

        -- Create directory (and parent directories) if it doesn't exist.
        liftIO $ Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent filepath

        -- To make sure there is exactly one blank line between the existing code
        -- and the new code that is being scaffolded, we remove any newlines from
        -- the end of the file and append the new code with a blank line before it.
        --
        -- If the file is empty (or doesn't exist), no blank line is added.
        existingContent <-
          ifM
            (liftIO $ Dir.doesFileExist $ SP.fromAbsFile filepath)
            (liftIO $ Text.readFile $ SP.fromAbsFile filepath)
            (pure "")
        let textToWrite =
              if Text.null existingContent
                then rendered
                else Text.dropWhileEnd (== '\n') existingContent <> "\n\n" <> rendered
        liftIO $ Text.writeFile (SP.fromAbsFile filepath) textToWrite

        -- VSCode doesn't send a file change notification after we modify the file.
        -- So we request here to update the export cache for the modified file.
        ServerM.sendToReactor $ do
          refreshExportsOfFiles [filepath]
          -- Update diagnostics for the wasp file
          updateMissingExtImportDiagnostics
          ServerM.handler $
            asks (^. State.waspFileUri) >>= \case
              Just uri -> publishDiagnostics uri
              Nothing -> pure ()

        notifyClientOfFileChanges args
        respond $ Right Aeson.Null
      _ -> respond $ Left $ makeInvalidParamsError $ Text.pack $ "Top-level step in path to ext import is not a decl: " ++ show pathToExtImport

    -- Displays a message to the client that a file changed, with a button to
    -- open the changed file.
    notifyClientOfFileChanges :: Args -> ServerM ()
    notifyClientOfFileChanges Args {..} = do
      let symbol = case symbolName of
            ExtImportModule _ -> "default export"
            ExtImportField name -> "export " <> name
      let message =
            LSP.ShowMessageRequestParams
              { _xtype = LSP.MtInfo,
                _message = Text.pack $ printf "Created %s in %s." symbol (SP.fromAbsFile filepath),
                _actions = Just [LSP.MessageActionItem "Open"]
              }
      void $
        LSP.sendRequest LSP.SWindowShowMessageRequest message $ \case
          Left err -> logM $ "Error showing message for file created: " <> show err
          Right (Just (LSP.MessageActionItem "Open")) -> do
            -- Client selected the "Open" button, ask it to display the file.
            let showDocument =
                  LSP.ShowDocumentParams
                    { _uri = LSP.filePathToUri $ SP.fromAbsFile filepath,
                      _external = Nothing,
                      _takeFocus = Just True,
                      _selection = Nothing
                    }
            void $ LSP.sendRequest LSP.SWindowShowDocument showDocument (const (pure ()))
          Right _ -> return ()

-- | Check if the scaffold command has a template available for the given args.
-- If this is false, running the command with these args will __definitely__
-- fail.
hasTemplateForArgs :: Args -> Bool
hasTemplateForArgs Args {..} = case P.fileExtension $ SP.toPathAbsFile filepath of
  Nothing -> False
  Just ext -> isRight $ templateFileFor pathToExtImport ext

-- | @getTemplateFor pathToExtImport extension@ finds the mustache template in
-- @data/Lsp/templates/ts@ and compiles it.
getTemplateFor :: (MonadIO m) => ExprPath -> String -> m (Either String Mustache.Template)
getTemplateFor exprPath ext = runExceptT $ do
  templatesDir <- liftIO getTemplatesDir
  templateFile <- (templatesDir SP.</>) <$> templateFileFor exprPath ext
  templateExists <- liftIO $ doesFileExist templateFile
  if templateExists
    then do
      compileResult <- liftIO $ Mustache.automaticCompile [SP.fromAbsDir templatesDir] (SP.fromAbsFile templateFile)
      case compileResult of
        -- Note: 'error' is used here because all templates should compile succesfully.
        Left err -> error $ printf "Compilation of template %s failed: %s" (SP.fromAbsFile templateFile) (show err)
        Right template -> return template
    else throwError $ printf "No scaffolding template for request: %s does not exist" (SP.fromAbsFile templateFile)

-- | Renders a mustache template to text.
--
-- This function is partial: if errors are encountered rendering the template,
-- @error@ is returned.
--
-- Removes leading and trailing newlines, except it leaves a single trailing
-- newline (or adds one if there wasn't already a trailing newline).
renderTemplate :: Mustache.Template -> Aeson.Value -> Text
renderTemplate template templateData =
  let (errs, text) = Mustache.checkedSubstituteValue template $ Mustache.toMustache templateData
   in if null errs
        then Text.dropAround (== '\n') text <> "\n"
        else error $ printf "Unexpected errors rendering template: " ++ show errs

data TemplatesDir

data Template

type TemplateFile = SP.Path' (SP.Rel TemplatesDir) (SP.File Template)

templatesDirInDataDir :: SP.Path' (SP.Rel Wasp.Data.DataDir) (SP.Dir TemplatesDir)
templatesDirInDataDir = [SP.reldir|Lsp/templates/ts|]

getTemplatesDir :: IO (SP.Path' SP.Abs (SP.Dir TemplatesDir))
getTemplatesDir = (SP.</> templatesDirInDataDir) <$> Wasp.Data.getAbsDataDirPath

templateFileFor ::
  (MonadError String m) =>
  -- | Path to the external import that the scaffold request came from.
  ExprPath ->
  -- | Extension of the file that the request is scaffolding code in.
  String ->
  m TemplateFile
templateFileFor [Decl "query" _, DictKey "fn"] ".ts" = pure [SP.relfile|query.fn.ts|]
templateFileFor [Decl "action" _, DictKey "fn"] ".ts" = pure [SP.relfile|action.fn.ts|]
templateFileFor [Decl declType _, DictKey "fn"] ".js"
  | declType `elem` ["query", "action"] = pure [SP.relfile|operation.fn.js|]
templateFileFor [Decl "page" _, DictKey "component"] ext
  | ext `elem` [".jsx", ".tsx"] = pure [SP.relfile|page.component.jsx|]
templateFileFor exprPath ext = throwError $ printf "No template defined for %s with extension %s" (show exprPath) ext
