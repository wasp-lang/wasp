{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Where an operation is available, keyed by surface: the client (default)
-- and MCP clients. Experimental.
module Wasp.AppSpec.Expose
  ( Expose (..),
    McpTool (..),
    isExposedToClient,
    getMcpTool,
    getMcpToolName,
    isDestructive,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Mcp (InsufficientScope)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.Util (camelToSnakeCase)

data Expose = Expose
  { client :: Maybe Bool,
    mcp :: Maybe McpTool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | How an operation appears as an MCP tool. A query's tool is read-only,
-- an action's isn't.
data McpTool = McpTool
  { -- | Defaults to the operation's name in snake_case (see 'getMcpToolName').
    name :: Maybe String,
    title :: Maybe String,
    -- | The scope (from @app.mcp.scopes@) a connection needs to see and call the tool.
    scope :: Maybe String,
    -- | Overrides @app.mcp.insufficientScope@ for this tool.
    insufficientScope :: Maybe InsufficientScope,
    description :: String,
    -- | A zod object schema for the tool's arguments.
    input :: ExtImport,
    -- | A zod schema describing the result. Never used to validate it.
    output :: Maybe ExtImport,
    idempotent :: Maybe Bool,
    openWorld :: Maybe Bool,
    destructive :: Maybe Bool,
    -- | Writes the confirm page's summary of a call. Set by
    -- @destructive: { describe }@, which also sets 'destructive'.
    describe :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Whether calls wait for the user's approval (`destructive: true` or `{ describe }`).
isDestructive :: McpTool -> Bool
isDestructive tool = tool.destructive == Just True

-- | Operations get an HTTP route and a client export unless they opt out.
isExposedToClient :: Maybe Expose -> Bool
isExposedToClient = maybe True (fromMaybe True . client)

getMcpTool :: Maybe Expose -> Maybe McpTool
getMcpTool expose = expose >>= mcp

-- | The tool's name: `expose.mcp.name`, or else the operation's name in
-- snake_case (@getLinks@ becomes @get_links@).
getMcpToolName :: String -> McpTool -> String
getMcpToolName operationName tool = fromMaybe (camelToSnakeCase operationName) tool.name
