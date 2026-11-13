// The endpoints the consent, connections and confirm pages call, in the
// user's session. The server serves them all through `mcp.user.handle`, and
// the browser components call them through an `McpAppCall`.
import type {
  AuthorizeRequestView,
  ConnectionView,
  PendingActionView,
  RedirectableAuthorizeError,
  ScopeView,
} from "./views";

export type McpAppEndpoints = {
  /** `query` is the consent page's query string: the authorization request. */
  "authorize/describe": {
    body: { query: string };
    result: AuthorizeRequestView | RedirectableAuthorizeError;
  };
  "authorize/approve": {
    body: { query: string; scopes: string[]; requireConfirmation: boolean };
    result: { redirectTo: string };
  };
  "authorize/deny": {
    body: { query: string };
    result: { redirectTo: string };
  };
  "connections/list": {
    body: Record<string, never>;
    result: { connections: ConnectionView[]; scopes: ScopeView[] };
  };
  "connections/update": {
    body: {
      clientId: string;
      scopes?: string[];
      requireConfirmation?: boolean;
    };
    result: ConnectionView;
  };
  "connections/revoke": {
    body: { clientId: string };
    result: Record<string, never>;
  };
  "actions/get": { body: { actionId: string }; result: PendingActionView };
  "actions/approve": { body: { actionId: string }; result: PendingActionView };
  "actions/deny": { body: { actionId: string }; result: PendingActionView };
};

export type McpAppEndpoint = keyof McpAppEndpoints;

/** POSTs `body` to the endpoint as the logged-in user and returns its result. */
export type McpAppCall = <E extends McpAppEndpoint>(
  endpoint: E,
  body: McpAppEndpoints[E]["body"],
) => Promise<McpAppEndpoints[E]["result"]>;
