// Turns `Authorization: Bearer <token>` into a principal. Permissions come
// from the connection at request time, not from the token, so unticking a
// scope or revoking a connection takes effect on the next call.
import type { McpPrincipal } from "./config";
import { unseal } from "./crypto";
import type { AccessTokenPayload } from "./oauth/token";
import type { Runtime } from "./runtime";
import type { ConnectionRecord } from "./store";

const LAST_USED_THROTTLE_MS = 60_000;

export function principalFromConnection(
  connection: ConnectionRecord,
): McpPrincipal {
  return {
    userId: connection.userId,
    connectionId: connection.id,
    clientId: connection.clientId,
    clientName: connection.clientName,
    scopes: connection.scopes,
    requireConfirmation: connection.requireConfirmation,
  };
}

function parseBearer(authorization: string | null | undefined): string | null {
  if (!authorization) return null;
  return /^Bearer\s+(\S+)\s*$/i.exec(authorization)?.[1] ?? null;
}

/** Null when the token is missing, unknown, expired, revoked, or for another resource. */
export async function authenticate(
  rt: Runtime,
  authorization: string | null | undefined,
): Promise<McpPrincipal | null> {
  const raw = parseBearer(authorization);
  if (!raw) return null;
  const now = rt.now();
  const token = unseal<AccessTokenPayload>(rt.keys.access, "access", raw);
  if (!token || token.exp <= now.getTime()) return null;
  if (token.resource !== rt.urls.mcpResource) return null;
  const connection = await rt.connections.getConnection(token.connectionId);
  if (!connection || connection.revokedAt) return null;

  if (
    !connection.lastUsedAt ||
    now.getTime() - connection.lastUsedAt.getTime() > LAST_USED_THROTTLE_MS
  ) {
    // Fire and forget: usage tracking must never slow down or fail a request.
    rt.connections
      .updateConnection(connection.id, { lastUsedAt: now })
      .catch(() => {});
  }
  return principalFromConnection(connection);
}
