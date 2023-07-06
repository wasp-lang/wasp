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
import Control.Monad (unless)
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
import Wasp.LSP.ServerM (ServerM)
import Wasp.LSP.TypeInference (ExprPath)
import qualified Wasp.LSP.TypeInference as Inference
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
    scaffold Args {..} = case P.fileExtension $ SP.toPathAbsFile filepath of
      Nothing -> respond $ Left $ invalidParams "Invalid filepath: no extension"
      Just ext ->
        getTemplateFor pathToExtImport ext >>= \case
          Left err ->
            respond $ Left $ invalidParams $ Text.pack err
          Right template -> do
            let symbolData = case symbolName of
                  ExtImportModule name -> ["default?" .= True, "named?" .= False, "name" .= name]
                  ExtImportField name -> ["default?" .= False, "named?" .= True, "name" .= name]
            -- TODO(before merge): get the decl name to here somehow
            let templateData = object $ symbolData ++ ["upperDeclName" .= ("UpperDeclName" :: Text)]
            let rendered = renderTemplate template templateData
            fileExists <- liftIO $ doesFileExist filepath
            logM $ printf "[wasp.scaffold.ts-symbol]: exists=%s rendered=%s" (show fileExists) (show rendered)
            -- TODO(before merge): use workspace edits
            unless fileExists $ liftIO $ Text.writeFile (SP.fromAbsFile filepath) ""
            liftIO $ Text.appendFile (SP.fromAbsFile filepath) rendered
            respond $ Right Aeson.Null

    invalidParams msg =
      LSP.ResponseError
        { _code = LSP.InvalidParams,
          _message = msg,
          _xdata = Nothing
        }

getTemplateFor :: MonadIO m => ExprPath -> String -> m (Either String Mustache.Template)
getTemplateFor exprPath ext = runExceptT $ do
  templatesDir <- liftIO getTemplatesDir
  templateFile <- (templatesDir SP.</>) <$> relTemplateForDeclType exprPath ext
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
relTemplateForDeclType :: MonadError String m => ExprPath -> String -> m (SP.Path' (SP.Rel TemplatesDir) (SP.File Template))
relTemplateForDeclType exprPath ext = case SP.parseRelFile $ pathDescription ++ ext of
  Nothing -> throwError $ "Invalid path from exprPath and ext: " ++ pathDescription ++ ext
  Just file -> return file
  where
    pathDescription = intercalate "." $ map stepDescription exprPath
    stepDescription (Inference.DictKey k) = printf "%s" k
    stepDescription Inference.List = "list"
    stepDescription (Inference.Tuple n) = printf "[%d]" n
    stepDescription (Inference.Decl decl) = decl
