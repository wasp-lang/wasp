// Connected apps. There's one connection per authorization, so a client
// installed twice or authorized again for more scopes has several; users see
// and manage them per client. Tokens hang off a connection and permissions
// are read from it on every request, so edits here apply on the next call.
import { capScopes, normalizeScopes } from "../../scopes";
import type { ConnectionView, ScopeView } from "../../views";
import { McpUserError } from "../errors";
import type { Runtime } from "../runtime";
import type { ConnectionRecord } from "../store";

const safeHost = (url: string) => {
  try {
    return new URL(url).hostname;
  } catch {
    return url;
  }
};

/** The app's scopes, for the consent and connections checkboxes. */
export const scopeViews = (rt: Runtime): ScopeView[] =>
  Object.entries(rt.scopes).map(([scope, d]) => ({
    scope,
    label: d.label,
    description: d.description,
    implies: [...(d.implies ?? [])],
  }));

const latest = (dates: (Date | null)[]): Date | null =>
  dates.reduce<Date | null>((a, b) => (!b || (a && a >= b) ? a : b), null);

/** One client's connections as one entry. `connections` is newest first and not empty. */
function toView(rt: Runtime, connections: ConnectionRecord[]): ConnectionView {
  const newest = connections[0]!;
  return {
    clientId: newest.clientId,
    clientName: newest.clientName,
    clientHost: safeHost(newest.clientId),
    // What the client can do through any of its connections.
    scopes: normalizeScopes(
      rt.scopes,
      connections.flatMap((c) => c.scopes),
    ),
    requestedScopes: normalizeScopes(
      rt.scopes,
      connections.flatMap((c) => c.requestedScopes),
    ),
    // Shown as on only if every connection asks first.
    requireConfirmation: connections.every((c) => c.requireConfirmation),
    createdAt: connections.at(-1)!.createdAt.toISOString(),
    lastUsedAt:
      latest(connections.map((c) => c.lastUsedAt))?.toISOString() ?? null,
  };
}

function groupByClient(connections: ConnectionRecord[]): ConnectionRecord[][] {
  const groups = new Map<string, ConnectionRecord[]>();
  for (const c of connections) {
    groups.set(c.clientId, [...(groups.get(c.clientId) ?? []), c]);
  }
  return [...groups.values()];
}

async function findClientConnections(
  rt: Runtime,
  userId: string,
  clientId: string,
): Promise<ConnectionRecord[]> {
  const connections = (
    await rt.connections.listActiveConnections(userId)
  ).filter((c) => c.clientId === clientId);
  if (connections.length === 0)
    throw new McpUserError(404, "Connected app not found.");
  return connections;
}

export async function listConnections(
  rt: Runtime,
  userId: string,
): Promise<ConnectionView[]> {
  const active = await rt.connections.listActiveConnections(userId);
  return groupByClient(active).map((group) => toView(rt, group));
}

/** Applies to every connection of the client. */
export async function updateConnection(
  rt: Runtime,
  userId: string,
  input: { clientId: string; scopes?: string[]; requireConfirmation?: boolean },
): Promise<ConnectionView> {
  const connections = await findClientConnections(rt, userId, input.clientId);
  const updated = await Promise.all(
    connections.map(async (connection) => {
      // Like consent: the user can narrow or restore, never exceed what the client asked for.
      const scopes = input.scopes
        ? capScopes(rt.scopes, input.scopes, connection.requestedScopes)
        : connection.scopes;
      const requireConfirmation =
        input.requireConfirmation ?? connection.requireConfirmation;
      await rt.connections.updateConnection(connection.id, {
        scopes,
        // Saved as read, so a connection made before the app had scopes
        // (read as asking for all of them) keeps the user's narrowing.
        requestedScopes: connection.requestedScopes,
        requireConfirmation,
      });
      return { ...connection, scopes, requireConfirmation };
    }),
  );
  return toView(rt, updated);
}

/** Disconnects the client: every one of its connections. */
export async function revokeConnection(
  rt: Runtime,
  userId: string,
  input: { clientId: string },
): Promise<void> {
  const connections = await findClientConnections(rt, userId, input.clientId);
  const now = rt.now();
  await Promise.all(
    connections.map((c) => rt.connections.revokeConnection(c.id, now)),
  );
}
