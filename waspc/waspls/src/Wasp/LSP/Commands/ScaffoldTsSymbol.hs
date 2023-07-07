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
    Args (Args, symbolName, pathToExtImport, filepath),
    hasTemplateForArgs,
    command,
    plugin,
  )
where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Log.Class (logM)
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON (toJSON), fromJSON, object, parseJSON, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import qualified Text.Mustache as Mustache
import Text.Printf (printf)
import Wasp.Analyzer.Parser.AST (ExtImportName (ExtImportField, ExtImportModule))
import qualified Wasp.Data
import Wasp.LSP.Commands.CommandPlugin (CommandPlugin (CommandPlugin, commandHandler, commandName))
import Wasp.LSP.ServerMonads (ServerM)
import Wasp.LSP.TypeInference (ExprPath)
import qualified Wasp.LSP.TypeInference as Inference
import Wasp.Util (toUpperFirst)
import Wasp.Util.IO (doesFileExist)

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

command :: Args -> LSP.Command
command args =
  LSP.Command
    { _title = "Scaffold TS Code",
      _command = commandName plugin,
      _arguments = Just $ LSP.List [toJSON args]
    }

plugin :: CommandPlugin
plugin =
  CommandPlugin
    { commandName = "wasp.scaffold.ts-symbol",
      commandHandler = handler
    }

handler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
handler request respond = case request ^. LSP.params . LSP.arguments of
  Just (LSP.List [argument]) -> case fromJSON argument of
    Error err -> respond $ Left $ invalidParams $ Text.pack err
    Success args -> scaffold args
  _ -> respond $ Left $ invalidParams "Expected exactly one argument"
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
    renderScaffoldTemplate Args {..} template = case pathToExtImport of
      Inference.Decl _ declName : _ -> do
        let symbolData = case symbolName of
              ExtImportModule name -> ["default?" .= True, "named?" .= False, "name" .= name]
              ExtImportField name -> ["default?" .= False, "named?" .= True, "name" .= name]
        let templateData = object $ symbolData ++ ["upperDeclName" .= toUpperFirst declName]
        let rendered = renderTemplate template templateData
        logM $ printf "[wasp.scaffold.ts-symbol]: rendered=%s" (show rendered)
        -- NOTE: due to waspls only being able to see* .wasp files, we can't go
        -- through the proper route of sending a WorkspaceApplyEdit request to
        -- the client. Modifying the file on disk is a workaround.
        --
        -- \* \"See\" meaning the LSP client sends waspls document sychronization
        -- notifications. The LSP spec does not appear to specify which documents
        -- this happens for: it seems to be up to the client to choose. In the case
        -- of waspls, this is set by the vscode extension.
        --
        -- We can easily configure the extension to send sync notifications for
        -- TS and JS files, but it would require a large shift in the structure
        -- of waspls to support handling events for multiple files. The best time
        -- for this to happen would be simultaneously with wasp file imports being
        -- implemented in the language and support added to waspls.
        liftIO $ Text.appendFile (SP.fromAbsFile filepath) rendered
        respond $ Right Aeson.Null
      _ -> respond $ Left $ invalidParams $ Text.pack $ "Top-level step in path to ext import is not a decl: " ++ show pathToExtImport

    invalidParams msg =
      LSP.ResponseError
        { _code = LSP.InvalidParams,
          _message = msg,
          _xdata = Nothing
        }

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
