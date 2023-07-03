{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Wasp.LSP.Commands.ScaffoldTsSymbol
  ( -- * Scaffold TS Symbol Command

    -- Command @wasp.scaffold.ts-symbol@ appends a new function with the given
    -- name to end of a particular file.
    Args (Args, symbolName, declType, filepath),
    command,
    plugin,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON (toJSON), fromJSON, object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseJSON)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified StrongPath as SP
import Wasp.Analyzer.Parser.AST (ExtImportName)
import Wasp.LSP.Commands.CommandPlugin (CommandPlugin (CommandPlugin, commandHandler, commandName))
import Wasp.LSP.ServerM (ServerM)

data Args = Args
  { -- | Name of the symbol to define. If this is 'ExtImportModule', it will
    -- create a function with the specified name and export it as default.
    symbolName :: ExtImportName,
    -- | The type of the declaration that this import is part of. Used to choose what kind of code to scaffold.
    declType :: String,
    -- | Path to the file to add the scaffoled code to the end of.
    filepath :: SP.Path' SP.Abs SP.File'
  }
  deriving (Show, Eq)

instance ToJSON Args where
  toJSON args =
    object
      [ "symbolName" .= symbolName args,
        "declType" .= declType args,
        "filepath" .= SP.toFilePath (filepath args)
      ]

instance FromJSON Args where
  parseJSON = withObject "Args" $ \v ->
    Args
      <$> v .: "symbolName"
      <*> v .: "declType"
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
    Error err ->
      respond $
        Left $
          LSP.ResponseError
            { _code = LSP.InvalidParams,
              _message = Text.pack err,
              _xdata = Nothing
            }
    Success args -> handle args
  _ ->
    respond $
      Left $
        LSP.ResponseError
          { _code = LSP.InvalidParams,
            _message = "Expected exactly one argument",
            _xdata = Nothing
          }
  where
    handle Args {..} = do
      respond $ Right Aeson.Null
