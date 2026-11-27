import type { McpAppEndpoints } from "../appEndpoints";
import type { AuthorizeParams } from "../views";
import { createAppEndpoints, isAppEndpoint } from "./appEndpoints";
import type { McpServerConfig } from "./config";
import { createConfirmations } from "./confirmations";
import { McpUserError } from "./errors";
import { createHttpHandler } from "./http";
import { toNodeMiddleware } from "./nodeAdapter";
import {
  approveAuthorization,
  denyAuthorization,
  describeAuthorizeRequest,
} from "./oauth/authorize";
import {
  listConnections,
  revokeConnection,
  updateConnection,
} from "./oauth/connections";
import {
  authorizationServerMetadata,
  protectedResourceMetadata,
} from "./oauth/metadata";
import { createRuntime } from "./runtime";
import { createMcpEndpoint } from "./server";

export {
  defineMcpTool,
  defineMcpToolFor,
  type AnyMcpTool,
  type InsufficientScopeBehavior,
  type McpCallContext,
  type McpDescribeResult,
  type McpLogEntry,
  type McpOperation,
  type McpOperationContext,
  type McpPrincipal,
  type McpServerConfig,
  type McpToolDefinition,
} from "./config";
export { McpToolError, McpUserError } from "./errors";
export { createMemoryStore } from "./memoryStore";
export {
  createPrismaStore,
  type McpPrismaClient,
  type McpPrismaDelegate,
  type McpPrismaStoreOptions,
} from "./prismaStore";
export { createMemoryRateLimitStore, type RateLimitStore } from "./rateLimit";
export type {
  ConnectionRecord,
  ConnectionStore,
  McpStore,
  PendingActionRecord,
  PendingActionStore,
} from "./store";

/**
 * The MCP feature for one app: the MCP endpoint and OAuth routes (`handle`,
 * `nodeMiddleware`), and the functions behind the consent, connections, and
 * confirm pages (`user`). Each user function takes the logged-in user's id;
 * the caller is responsible for the session.
 */
export function createWaspMcp<User>(config: McpServerConfig<User>) {
  const rt = createRuntime(config);
  const confirmations = createConfirmations(rt);
  const http = createHttpHandler(rt, createMcpEndpoint(rt, confirmations));
  const appEndpoints = createAppEndpoints(rt, confirmations);

  return {
    /** Web-standard handler: a Response for MCP routes, null for anything else. */
    handle: http.handle,
    /** Express/Node middleware for the same routes. */
    nodeMiddleware: toNodeMiddleware(http, rt.urls.issuer),

    user: {
      /**
       * Serves one of the `McpAppEndpoints` the browser components call, with
       * the body validated. Throws McpUserError (with a status code) for an
       * unknown endpoint, a bad body, or a missing record.
       */
      handle: async (
        userId: string,
        endpoint: string,
        body: unknown,
      ): Promise<McpAppEndpoints[keyof McpAppEndpoints]["result"]> => {
        if (!isAppEndpoint(appEndpoints, endpoint))
          throw new McpUserError(404, "Not found.");
        return appEndpoints[endpoint](userId, body);
      },

      describeAuthorizeRequest: (userId: string, params: AuthorizeParams) =>
        describeAuthorizeRequest(rt, userId, params),
      approveAuthorization: (
        userId: string,
        input: {
          params: AuthorizeParams;
          scopes: string[];
          requireConfirmation: boolean;
        },
      ) => approveAuthorization(rt, userId, input),
      denyAuthorization: (
        _userId: string,
        input: { params: AuthorizeParams },
      ) => denyAuthorization(rt, input),

      listConnections: (userId: string) => listConnections(rt, userId),
      updateConnection: (
        userId: string,
        input: {
          clientId: string;
          scopes?: string[];
          requireConfirmation?: boolean;
        },
      ) => updateConnection(rt, userId, input),
      revokeConnection: (userId: string, input: { clientId: string }) =>
        revokeConnection(rt, userId, input),

      getPendingAction: (userId: string, input: { actionId: string }) =>
        confirmations.getPendingAction(userId, input.actionId),
      approvePendingAction: (userId: string, input: { actionId: string }) =>
        confirmations.approvePendingAction(userId, input.actionId),
      denyPendingAction: (userId: string, input: { actionId: string }) =>
        confirmations.denyPendingAction(userId, input.actionId),
    },

    metadata: {
      protectedResource: () => protectedResourceMetadata(rt),
      authorizationServer: () => authorizationServerMetadata(rt),
    },
    urls: rt.urls,
  };
}

export type WaspMcp = ReturnType<typeof createWaspMcp>;
