// Wasp adapter: turns an operation's context into a Principal.
import { HttpError } from "wasp/server";
import type { McpCallContext } from "wasp/server/mcp";
import {
  principalFromMcp,
  principalFromSession,
  type Principal,
} from "./principal";

// `mcp` is set when the operation runs as an MCP tool.
type OperationContext = {
  user?: { id: string } | null;
  mcp?: McpCallContext;
};

/** For operations only the dashboard calls: the session user gets every scope. */
export function sessionPrincipal(context: OperationContext): Principal {
  if (!context.user) throw new HttpError(401, "Log in to continue.");
  return principalFromSession(context.user);
}

/**
 * For operations that are also MCP tools: an agent gets the scopes its
 * connection was granted, and the dashboard keeps every scope. Wasp already
 * hides tools whose scope wasn't granted; the services check again.
 */
export function callerPrincipal(context: OperationContext): Principal {
  if (!context.user) throw new HttpError(401, "Log in to continue.");
  const mcp = context.mcp;
  return mcp
    ? principalFromMcp({
        user: context.user,
        scopes: mcp.scopes,
        connection: mcp.connection,
      })
    : principalFromSession(context.user);
}
