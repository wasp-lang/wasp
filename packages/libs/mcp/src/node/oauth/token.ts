// POST /mcp/oauth/token: authorization_code (with PKCE) and refresh_token
// (single-use rotation with reuse detection), in RFC 6749 shapes. Plus
// POST /mcp/oauth/revoke (RFC 7009), which always succeeds so callers can't probe tokens.
//
// Codes and tokens are sealed (crypto.ts), never stored. Each code carries the
// id of the connection it creates, so a second use finds it already there.
// A refresh token carries the connection's `refreshGeneration` it expects, and
// using it moves the counter on (a compare-and-set), so a second use finds the
// wrong generation. Either is treated as theft and ends the connection.
import { pkceChallenge, seal, unseal } from "../crypto";
import type { Runtime } from "../runtime";

export const ACCESS_TOKEN_TTL_S = 3600;
export const REFRESH_TOKEN_TTL_MS = 30 * 24 * 60 * 60 * 1000;

/** Inside a sealed authorization code. Times are epoch milliseconds. */
export type AuthCodePayload = {
  /** The connection the exchange creates: one per authorization. */
  connectionId: string;
  userId: string;
  clientId: string;
  clientName: string;
  redirectUri: string;
  codeChallenge: string;
  scopes: string[];
  requestedScopes: string[];
  resource: string;
  requireConfirmation: boolean;
  exp: number;
};

/** Inside a sealed access token. The connection is loaded on every call, so revoking takes effect on the next one. */
export type AccessTokenPayload = {
  connectionId: string;
  resource: string;
  exp: number;
};

/** Inside a sealed refresh token. */
export type RefreshTokenPayload = {
  connectionId: string;
  generation: number;
  resource: string;
  exp: number;
};

export type TokenResult =
  | {
      status: 200;
      body: {
        access_token: string;
        token_type: "Bearer";
        expires_in: number;
        refresh_token: string;
        scope: string;
      };
    }
  | { status: 400 | 401; body: { error: string; error_description: string } };

const oauthError = (
  error: string,
  error_description: string,
  status: 400 | 401 = 400,
): TokenResult => ({
  status,
  body: { error, error_description },
});

type Params = Record<string, string | undefined>;

export async function exchangeToken(
  rt: Runtime,
  params: Record<string, unknown>,
): Promise<TokenResult> {
  const p: Params = Object.fromEntries(
    Object.entries(params).map(([k, v]) => [
      k,
      typeof v === "string" ? v : undefined,
    ]),
  );
  switch (p.grant_type) {
    case "authorization_code":
      return exchangeCode(rt, p);
    case "refresh_token":
      return refresh(rt, p);
    case undefined:
      return oauthError("invalid_request", "grant_type is required.");
    default:
      return oauthError(
        "unsupported_grant_type",
        "Supported grant types: authorization_code, refresh_token.",
      );
  }
}

async function exchangeCode(rt: Runtime, p: Params): Promise<TokenResult> {
  const now = rt.now();
  if (!p.code || !p.redirect_uri || !p.client_id || !p.code_verifier) {
    return oauthError(
      "invalid_request",
      "code, redirect_uri, client_id, and code_verifier are required.",
    );
  }
  const code = unseal<AuthCodePayload>(rt.keys.code, "code", p.code);
  if (!code)
    return oauthError("invalid_grant", "The authorization code is invalid.");
  if (code.exp <= now.getTime())
    return oauthError("invalid_grant", "The authorization code has expired.");
  if (code.clientId !== p.client_id)
    return oauthError(
      "invalid_grant",
      "client_id doesn't match the authorization request.",
    );
  if (code.redirectUri !== p.redirect_uri)
    return oauthError(
      "invalid_grant",
      "redirect_uri doesn't match the authorization request.",
    );
  // Checked before single use, so replaying a stolen code without its verifier
  // can't end the victim's connection.
  if (pkceChallenge(p.code_verifier) !== code.codeChallenge)
    return oauthError("invalid_grant", "PKCE verification failed.");

  const created = await rt.connections.createConnection({
    id: code.connectionId,
    userId: code.userId,
    clientId: code.clientId,
    clientName: code.clientName,
    scopes: code.scopes,
    requestedScopes: code.requestedScopes,
    requireConfirmation: code.requireConfirmation,
    createdAt: now,
  });
  if (!created) {
    await rt.connections.revokeConnection(code.connectionId, now);
    return oauthError(
      "invalid_grant",
      "The authorization code was already used. The connection it created has been revoked.",
    );
  }
  return issueTokens(rt, {
    connectionId: created.id,
    generation: created.refreshGeneration,
    scopes: code.scopes,
    resource: code.resource,
    now,
  });
}

async function refresh(rt: Runtime, p: Params): Promise<TokenResult> {
  const now = rt.now();
  if (!p.refresh_token)
    return oauthError("invalid_request", "refresh_token is required.");
  const token = unseal<RefreshTokenPayload>(
    rt.keys.refresh,
    "refresh",
    p.refresh_token,
  );
  if (!token)
    return oauthError("invalid_grant", "The refresh token is invalid.");
  if (token.exp <= now.getTime())
    return oauthError("invalid_grant", "The refresh token has expired.");
  const connection = await rt.connections.getConnection(token.connectionId);
  if (!connection || connection.revokedAt)
    return oauthError("invalid_grant", "The connection was revoked.");
  if (p.client_id && p.client_id !== connection.clientId)
    return oauthError("invalid_grant", "client_id doesn't match.");
  if (p.resource && p.resource.replace(/\/+$/, "") !== token.resource) {
    return oauthError(
      "invalid_target",
      "A refresh token can only mint tokens for its original resource.",
    );
  }
  if (
    !(await rt.connections.advanceGeneration(connection.id, token.generation))
  ) {
    // A rotated refresh token came back: assume it was stolen and end the connection.
    await rt.connections.revokeConnection(connection.id, now);
    return oauthError(
      "invalid_grant",
      "The refresh token was already used. The connection has been revoked; authorize again.",
    );
  }
  return issueTokens(rt, {
    connectionId: connection.id,
    generation: token.generation + 1,
    scopes: connection.scopes,
    resource: token.resource,
    now,
  });
}

function issueTokens(
  rt: Runtime,
  g: {
    connectionId: string;
    generation: number;
    scopes: string[];
    resource: string;
    now: Date;
  },
): TokenResult {
  const now = g.now.getTime();
  const access: AccessTokenPayload = {
    connectionId: g.connectionId,
    resource: g.resource,
    exp: now + ACCESS_TOKEN_TTL_S * 1000,
  };
  const refresh: RefreshTokenPayload = {
    connectionId: g.connectionId,
    generation: g.generation,
    resource: g.resource,
    exp: now + REFRESH_TOKEN_TTL_MS,
  };
  return {
    status: 200,
    body: {
      access_token: seal(rt.keys.access, "access", access),
      token_type: "Bearer",
      expires_in: ACCESS_TOKEN_TTL_S,
      refresh_token: seal(rt.keys.refresh, "refresh", refresh),
      scope: g.scopes.join(" "),
    },
  };
}

/**
 * Revoking either token ends the connection: a single sealed access token
 * can't be revoked on its own, and a client revoking a token is disconnecting.
 */
export async function revokeToken(
  rt: Runtime,
  token: string | undefined,
): Promise<void> {
  if (!token) return;
  const payload =
    unseal<AccessTokenPayload>(rt.keys.access, "access", token) ??
    unseal<RefreshTokenPayload>(rt.keys.refresh, "refresh", token);
  if (payload)
    await rt.connections.revokeConnection(payload.connectionId, rt.now());
}
