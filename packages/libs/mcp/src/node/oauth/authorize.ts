// The authorization request. The consent page is the authorization endpoint,
// so the request arrives as its query params and is validated again on approve:
// nothing needs to be stored before the user decides.
import { randomUUID } from "node:crypto";
import { expandScopes, normalizeScopes } from "../../scopes";
import type {
  AuthorizeParams,
  AuthorizeRequestView,
  RedirectableAuthorizeError,
} from "../../views";
import { seal } from "../crypto";
import type { Runtime } from "../runtime";
import { InvalidClientError, redirectUriMatches, resolveClient } from "./cimd";
import { scopeViews } from "./connections";
import type { AuthCodePayload } from "./token";

export const CODE_TTL_MS = 60_000;

type ValidRequest = {
  ok: true;
  clientId: string;
  clientName: string;
  clientHost: string;
  redirectUri: string;
  codeChallenge: string;
  state?: string;
  resource: string;
  requestedScopes: string[];
};

export function authorizeRedirect(
  rt: Runtime,
  redirectUri: string,
  params: Record<string, string | undefined>,
): string {
  const url = new URL(redirectUri);
  for (const [k, v] of Object.entries({ ...params, iss: rt.urls.issuer })) {
    if (v !== undefined) url.searchParams.set(k, v);
  }
  return url.toString();
}

/**
 * A bad `client_id` or `redirect_uri` throws InvalidClientError: the page shows
 * it and never redirects, since the redirect target itself can't be trusted.
 * Other problems are returned as redirectable errors.
 */
async function validate(
  rt: Runtime,
  params: AuthorizeParams,
): Promise<ValidRequest | RedirectableAuthorizeError> {
  if (!params.client_id)
    throw new InvalidClientError("The request is missing client_id.");
  const client = await resolveClient(rt, params.client_id);
  if (!params.redirect_uri)
    throw new InvalidClientError("The request is missing redirect_uri.");
  if (!redirectUriMatches(params.redirect_uri, client.redirectUris)) {
    throw new InvalidClientError(
      "redirect_uri isn't one of the redirect URIs in the client's metadata document.",
    );
  }

  const fail = (
    error: string,
    errorDescription: string,
  ): RedirectableAuthorizeError => ({
    ok: false,
    error,
    errorDescription,
    redirectTo: authorizeRedirect(rt, params.redirect_uri!, {
      error,
      error_description: errorDescription,
      state: params.state,
    }),
  });

  if (params.response_type !== "code")
    return fail(
      "unsupported_response_type",
      "Only response_type=code is supported.",
    );
  if (
    !params.code_challenge ||
    !/^[A-Za-z0-9_-]{43,128}$/.test(params.code_challenge)
  ) {
    return fail("invalid_request", "PKCE is required: send a code_challenge.");
  }
  if (params.code_challenge_method !== "S256")
    return fail("invalid_request", "code_challenge_method must be S256.");

  let requestedScopes: string[];
  if (params.scope?.trim()) {
    const requested = params.scope.trim().split(/\s+/);
    const unknown = requested.filter((s) => !(s in rt.scopes));
    if (unknown.length)
      return fail("invalid_scope", `Unknown scope: ${unknown.join(", ")}.`);
    requestedScopes = expandScopes(rt.scopes, requested);
  } else {
    requestedScopes = rt.initialScopes;
  }

  const resource = params.resource
    ? params.resource.replace(/\/+$/, "")
    : rt.urls.mcpResource;
  if (resource !== rt.urls.mcpResource)
    return fail("invalid_target", `resource must be ${rt.urls.mcpResource}.`);

  return {
    ok: true,
    clientId: client.clientId,
    clientName: client.clientName,
    clientHost: client.clientHost,
    redirectUri: params.redirect_uri,
    codeChallenge: params.code_challenge,
    state: params.state,
    resource,
    requestedScopes,
  };
}

/** What the consent page shows: the request, and this user's newest connection to the client. */
export async function describeAuthorizeRequest(
  rt: Runtime,
  userId: string,
  params: AuthorizeParams,
): Promise<AuthorizeRequestView | RedirectableAuthorizeError> {
  const req = await validate(rt, params);
  if (!req.ok) return req;
  const connection = await rt.connections.findActiveConnection(
    userId,
    req.clientId,
  );
  return {
    ok: true,
    clientId: req.clientId,
    clientName: req.clientName,
    clientHost: req.clientHost,
    scopes: scopeViews(rt),
    requestedScopes: req.requestedScopes,
    existingConnection: connection
      ? {
          scopes: connection.scopes,
          requireConfirmation: connection.requireConfirmation,
        }
      : null,
  };
}

export async function approveAuthorization(
  rt: Runtime,
  userId: string,
  input: {
    params: AuthorizeParams;
    scopes: string[];
    requireConfirmation: boolean;
  },
): Promise<{ redirectTo: string }> {
  const req = await validate(rt, input.params);
  if (!req.ok) return { redirectTo: req.redirectTo };

  // The user may tick more than the client asked for (a client starting
  // read-only, say), but never undeclared scopes.
  const scopes = expandScopes(rt.scopes, input.scopes);
  // Sealed, not stored. It carries the id of the connection it creates, so it
  // can be exchanged once (token.ts).
  const payload: AuthCodePayload = {
    connectionId: randomUUID(),
    userId,
    clientId: req.clientId,
    clientName: req.clientName,
    redirectUri: req.redirectUri,
    codeChallenge: req.codeChallenge,
    scopes,
    // The ceiling for later edits on the connections page.
    requestedScopes: normalizeScopes(rt.scopes, [
      ...req.requestedScopes,
      ...scopes,
    ]),
    resource: req.resource,
    requireConfirmation: input.requireConfirmation,
    exp: rt.now().getTime() + CODE_TTL_MS,
  };
  const code = seal(rt.keys.code, "code", payload);
  return {
    redirectTo: authorizeRedirect(rt, req.redirectUri, {
      code,
      state: req.state,
    }),
  };
}

export async function denyAuthorization(
  rt: Runtime,
  input: { params: AuthorizeParams },
): Promise<{ redirectTo: string }> {
  const req = await validate(rt, input.params);
  if (!req.ok) return { redirectTo: req.redirectTo };
  return {
    redirectTo: authorizeRedirect(rt, req.redirectUri, {
      error: "access_denied",
      error_description: "The user denied the request.",
      state: req.state,
    }),
  };
}
