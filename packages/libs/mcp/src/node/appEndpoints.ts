// Serves the McpAppEndpoints (../appEndpoints.ts) for the app's session-
// authenticated router: validates the body, then calls the user function.
import { z } from "zod";
import type { McpAppEndpoint, McpAppEndpoints } from "../appEndpoints";
import type { AuthorizeParams } from "../views";
import type { Confirmations } from "./confirmations";
import { McpUserError } from "./errors";
import {
  approveAuthorization,
  denyAuthorization,
  describeAuthorizeRequest,
} from "./oauth/authorize";
import {
  listConnections,
  revokeConnection,
  scopeViews,
  updateConnection,
} from "./oauth/connections";
import type { Runtime } from "./runtime";

const query = z.string();
const scopes = z.array(z.string());
const actionBody = z.object({ actionId: z.string() });

const AUTHORIZE_PARAMS = [
  "response_type",
  "client_id",
  "redirect_uri",
  "code_challenge",
  "code_challenge_method",
  "state",
  "scope",
  "resource",
] as const satisfies readonly (keyof AuthorizeParams)[];

/** The authorization request from the consent page's query string. */
function parseAuthorizeParams(search: string): AuthorizeParams {
  const all = new URLSearchParams(search);
  const params: AuthorizeParams = {};
  for (const key of AUTHORIZE_PARAMS) {
    const value = all.get(key);
    if (value !== null) params[key] = value;
  }
  return params;
}

type Handlers = {
  [E in McpAppEndpoint]: (
    userId: string,
    body: unknown,
  ) => Promise<McpAppEndpoints[E]["result"]>;
};

export function createAppEndpoints(
  rt: Runtime,
  confirmations: Confirmations,
): Handlers {
  return {
    "authorize/describe": (userId, body) => {
      const b = parse(z.object({ query }), body);
      return describeAuthorizeRequest(
        rt,
        userId,
        parseAuthorizeParams(b.query),
      );
    },
    "authorize/approve": (userId, body) => {
      const b = parse(
        z.object({ query, scopes, requireConfirmation: z.boolean() }),
        body,
      );
      return approveAuthorization(rt, userId, {
        params: parseAuthorizeParams(b.query),
        scopes: b.scopes,
        requireConfirmation: b.requireConfirmation,
      });
    },
    "authorize/deny": async (_userId, body) => {
      const b = parse(z.object({ query }), body);
      return denyAuthorization(rt, { params: parseAuthorizeParams(b.query) });
    },
    "connections/list": async (userId) => ({
      connections: await listConnections(rt, userId),
      scopes: scopeViews(rt),
    }),
    "connections/update": (userId, body) =>
      updateConnection(
        rt,
        userId,
        parse(
          z.object({
            clientId: z.string(),
            scopes: scopes.optional(),
            requireConfirmation: z.boolean().optional(),
          }),
          body,
        ),
      ),
    "connections/revoke": async (userId, body) => {
      await revokeConnection(
        rt,
        userId,
        parse(z.object({ clientId: z.string() }), body),
      );
      return {};
    },
    "actions/get": (userId, body) =>
      confirmations.getPendingAction(userId, parse(actionBody, body).actionId),
    "actions/approve": (userId, body) =>
      confirmations.approvePendingAction(
        userId,
        parse(actionBody, body).actionId,
      ),
    "actions/deny": (userId, body) =>
      confirmations.denyPendingAction(userId, parse(actionBody, body).actionId),
  };
}

function parse<T>(schema: z.ZodType<T>, body: unknown): T {
  const result = schema.safeParse(body ?? {});
  if (!result.success) throw new McpUserError(400, "Invalid request body.");
  return result.data;
}

export const isAppEndpoint = (
  handlers: Handlers,
  name: string,
): name is McpAppEndpoint => Object.hasOwn(handlers, name);
