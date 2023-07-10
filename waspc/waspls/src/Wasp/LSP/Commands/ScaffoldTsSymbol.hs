{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Wasp.LSP.Commands.ScaffoldTsSymbol
  ( -- * Scaffold TS Symbol Command

    -- Command @wasp.scaffold.ts-symbol@ appends a new function with the given
    -- name to end of a particular file.
    --
    -- Scaffolding is based on the location of the external import. It looks for
    -- the templates for each kind of JS/TS code in @data/lsp/templates/ts@.
    -- For example, it differentiates between
    --
    -- @
    -- query getAll { fn: import { getAll } from "@server/queries.js" }
    -- @
    --
    -- and
    --
    -- @
    -- action deleteAll { fn: import { deleteAll } from "@server/queries.ts" }
    -- @
    --
    -- The former looks for a template named @query.fn.js@, the latter for
    -- @action.fn.ts@. To add support for a new kind of JS/TS code to scaffold,
    -- create a new file @data/lsp/templates/ts/<path>.<ext>@, where @<ext>@ is
    -- the extension of the file that the code will be added to and @<path>@ is
    -- the dot-separated path to the external import in the wasp code. Each
    -- piece of the path is:
    --
    -- - The declaration type, as a string.
    -- - @list@, for a value inside a list.
    -- - @[n]@, for the @n@-th value in a tuple.
    -- - The key of a dictionary field, as a string.
    --
    -- The path pieces are listed from outermost to innermost.
    --
    -- The templates have several variables available to them:
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
    plugin,
  )
where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Log.Class (logM)
import Data.Aeson (FromJSON, ToJSON (toJSON), object, parseJSON, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import qualified Text.Mustache as Mustache
import Text.Printf (printf)
import Wasp.Analyzer.Parser.AST (ExtImportName (ExtImportField, ExtImportModule))
import qualified Wasp.Data
import Wasp.LSP.Commands.CommandPlugin (CommandPlugin (CommandPlugin, commandHandler, commandName), invalidParams, withParsedArgs)
import Wasp.LSP.ServerMonads (ServerM)
import Wasp.LSP.TypeInference (ExprPath)
import qualified Wasp.LSP.TypeInference as Inference
import Wasp.Util (toUpperFirst)
import Wasp.Util.IO (doesFileExist)

plugin :: CommandPlugin
plugin =
  CommandPlugin
    { commandName = "wasp.scaffold.ts-symbol",
      commandHandler = handler
    }

command :: Args -> LSP.Command
command args =
  LSP.Command
    { _title = "Scaffold TS Code",
      _command = commandName plugin,
      _arguments = Just $ LSP.List [toJSON args]
    }

data Args = Args
  { -- | Name of the symbol to define. If this is 'ExtImportModule', it will
    -- create a function with the specified name and export it as default.
    symbolName :: ExtImportName,
    -- | Description of where the external import occurs in the source code, used,
    -- along with the extension of 'filepath', to determine what template to use.
    pathToExtImport :: ExprPath,
    -- | Path to the file to add the scaffoled code to the end of.
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
      Nothing -> respond $ Left $ invalidParams "Invalid filepath: no extension"
      Just ext ->
        getTemplateFor pathToExtImport ext >>= \case
          Left err ->
            respond $ Left $ invalidParams $ Text.pack err
          Right template -> renderScaffoldTemplate args template

    renderScaffoldTemplate :: Args -> Mustache.Template -> ServerM ()
    renderScaffoldTemplate args@Args {..} template = case pathToExtImport of
      Inference.Decl _ declName : _ -> do
        let symbolData = case symbolName of
              ExtImportModule name -> ["default?" .= True, "named?" .= False, "name" .= name]
              ExtImportField name -> ["default?" .= False, "named?" .= True, "name" .= name]
        let templateData = object $ symbolData ++ ["upperDeclName" .= toUpperFirst declName]

        let rendered = renderTemplate template templateData
        logM $ printf "[wasp.scaffold.ts-symbol]: rendered=%s" (show rendered)

        -- NOTE: we modify the file on disk instead of applying an edit through
        -- the LSP client. See "Current Limitations" above.
        liftIO $ Text.appendFile (SP.fromAbsFile filepath) rendered

        notifyClientOfChanges args
        respond $ Right Aeson.Null
      _ -> respond $ Left $ invalidParams $ Text.pack $ "Top-level step in path to ext import is not a decl: " ++ show pathToExtImport

    -- Displays a message to the client that a file changed, with a button to
    -- open the changed file.
    notifyClientOfChanges :: Args -> ServerM ()
    notifyClientOfChanges Args {..} = do
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
hasTemplateForArgs :: MonadIO m => Args -> m Bool
hasTemplateForArgs Args {..} = case P.fileExtension $ SP.toPathAbsFile filepath of
  Nothing -> return False
  Just ext -> isRight <$> getTemplateFor pathToExtImport ext

-- | @getTemplateFor pathToExtImport extension@ finds the mustache template in
-- @data/lsp/templates/ts@ and compiles it.
getTemplateFor :: MonadIO m => ExprPath -> String -> m (Either String Mustache.Template)
getTemplateFor exprPath ext = runExceptT $ do
  templatesDir <- liftIO getTemplatesDir
  templateFile <- (templatesDir SP.</>) <$> relTemplateForPath exprPath ext
  templateExists <- liftIO $ doesFileExist templateFile
  if templateExists
    then do
      compileResult <- liftIO $ Mustache.automaticCompile [SP.fromAbsDir templatesDir] (SP.fromAbsFile templateFile)
      case compileResult of
        -- Note: 'error' is used here because all templates should be valid.
        Left err -> error $ printf "Compilation of template %s failed: %s" (SP.fromAbsFile templateFile) (show err)
        Right template -> return template
    else throwError $ printf "No scaffolding template for request: %s does not exist" (SP.fromAbsFile templateFile)

-- | Renders a mustache template to text.
--
-- This function is partial: if errors are encountered rendering the template,
-- @error@ is returned.
renderTemplate :: Mustache.Template -> Aeson.Value -> Text
renderTemplate template templateData =
  let (errs, text) = Mustache.checkedSubstituteValue template $ Mustache.toMustache templateData
   in if null errs
        then text
        else error $ printf "Unexpected errors rendering template: " ++ show errs

data TemplatesDir

data Template

templatesDirInDataDir :: SP.Path' (SP.Rel Wasp.Data.DataDir) (SP.Dir TemplatesDir)
templatesDirInDataDir = [SP.reldir|lsp/templates/ts|]

getTemplatesDir :: IO (SP.Path' SP.Abs (SP.Dir TemplatesDir))
getTemplatesDir = (SP.</> templatesDirInDataDir) <$> Wasp.Data.getAbsDataDirPath

-- | @relTemplateForDeclType exprPath extension@ returns the relative path to the
-- scaffold template for the expr path and file type.
relTemplateForPath :: MonadError String m => ExprPath -> String -> m (SP.Path' (SP.Rel TemplatesDir) (SP.File Template))
relTemplateForPath exprPath ext = case SP.parseRelFile $ pathDescription ++ ext of
  Nothing -> throwError $ "Invalid path from exprPath and ext: " ++ pathDescription ++ ext
  Just file -> return file
  where
    pathDescription = intercalate "." $ map stepDescription exprPath
    stepDescription (Inference.DictKey k) = printf "%s" k
    stepDescription Inference.List = "list"
    stepDescription (Inference.Tuple n) = printf "[%d]" n
    stepDescription (Inference.Decl decl _) = decl
