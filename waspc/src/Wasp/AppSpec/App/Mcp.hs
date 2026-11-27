{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The app's MCP server: the `/mcp` endpoint and the OAuth server MCP
-- clients use to connect to a user's account. Experimental.
module Wasp.AppSpec.App.Mcp
  ( Mcp (..),
    McpScope (..),
    McpRateLimit (..),
    InsufficientScope (..),
    isWhoamiEnabled,
    getScopes,
    connectionEntityName,
    pendingActionEntityName,
    rateLimitEntityName,
    mcpEntityNames,
    mcpConnectionsFieldOnUserEntityName,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Route (Route)
import Prelude hiding (max)

data Mcp = Mcp
  { instructions :: String,
    -- | Where the app shows the consent form. Also the OAuth authorization endpoint.
    consentRoute :: Ref Route,
    -- | Where the app shows the confirm page for destructive tool calls.
    confirmRoute :: Maybe (Ref Route),
    whoami :: Maybe Bool,
    -- | Permissions users can grant a client, in declaration order (the
    -- consent screen's order).
    scopes :: Maybe [McpScope],
    -- | The scopes clients ask for when they first connect. Defaults to every scope.
    initialScopes :: Maybe [String],
    -- | What clients see of tools whose scope they weren't granted. Defaults to 'StepUp'.
    insufficientScope :: Maybe InsufficientScope,
    rateLimit :: Maybe McpRateLimit
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | @rateLimit: false@ in the spec is @enabled = False@.
data McpRateLimit = McpRateLimit
  { enabled :: Bool,
    windowMs :: Maybe Integer,
    max :: Maybe Integer
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data InsufficientScope
  = -- | Listed; calling it sends the user back to consent for its scope.
    StepUp
  | -- | Not listed for that client.
    Hide
  deriving (Show, Eq, Data, Generic)

instance FromJSON InsufficientScope where
  parseJSON = withText "InsufficientScope" $ \case
    "stepUp" -> pure StepUp
    "hide" -> pure Hide
    other -> fail $ "Unknown insufficientScope: " ++ show other

instance ToJSON InsufficientScope where
  toJSON StepUp = Aeson.String "stepUp"
  toJSON Hide = Aeson.String "hide"

data McpScope = McpScope
  { -- | E.g. @tasks:read@.
    name :: String,
    label :: String,
    description :: Maybe String,
    -- | Scopes granted along with this one.
    implies :: Maybe [String]
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

isWhoamiEnabled :: Mcp -> Bool
isWhoamiEnabled mcp = fromMaybe True mcp.whoami

getScopes :: Mcp -> [McpScope]
getScopes mcp = fromMaybe [] mcp.scopes

-- | The entities Wasp injects into the Prisma schema for MCP (see
-- Wasp.Generator.DbGenerator.Mcp).
connectionEntityName :: String
connectionEntityName = "McpConnection"

pendingActionEntityName :: String
pendingActionEntityName = "McpPendingAction"

rateLimitEntityName :: String
rateLimitEntityName = "McpRateLimit"

-- | Names users can't give their own entities when MCP is on.
mcpEntityNames :: [String]
mcpEntityNames = [connectionEntityName, pendingActionEntityName, rateLimitEntityName]

-- | The relation Wasp adds to the user entity.
mcpConnectionsFieldOnUserEntityName :: String
mcpConnectionsFieldOnUserEntityName = "mcpConnections"
