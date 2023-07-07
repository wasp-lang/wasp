{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Wasp.LSP.Commands.ScaffoldTsSymbol
  ( -- * Scaffold TS Symbol Command

    -- Command @wasp.scaffold.ts-symbol@ appends a new function with the given
    -- name to end of a particular file.
    Args (Args, symbolName, pathToExtImport, filepath),
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

getTemplateFor :: MonadIO m => ExprPath -> String -> m (Either String Mustache.Template)
getTemplateFor exprPath ext = runExceptT $ do
  templatesDir <- liftIO getTemplatesDir
  templateFile <- (templatesDir SP.</>) <$> relTemplateForPath exprPath ext
  templateExists <- liftIO $ doesFileExist templateFile
  if templateExists
    then do
      compileResult <- liftIO $ Mustache.automaticCompile [SP.fromAbsDir templatesDir] (SP.fromAbsFile templateFile)
      case compileResult of
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
