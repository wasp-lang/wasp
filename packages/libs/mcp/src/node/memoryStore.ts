// An McpStore that keeps everything in memory. For tests and local experiments:
// it loses all connections when the process exits.
import { randomUUID } from "node:crypto";
import type {
  ConnectionRecord,
  ConnectionStore,
  PendingActionRecord,
  PendingActionStore,
} from "./store";

export function createMemoryStore(): {
  connections: ConnectionStore;
  pendingActions: PendingActionStore;
  /** Read-only view of the stored records, for tests. */
  readonly records: {
    connections: Map<string, ConnectionRecord>;
    pendingActions: Map<string, PendingActionRecord>;
  };
} {
  const connections = new Map<string, ConnectionRecord>();
  const pendingActions = new Map<string, PendingActionRecord>();
  const copy = <T>(record: T | undefined): T | null =>
    record ? { ...record } : null;

  const connectionStore: ConnectionStore = {
    async getConnection(id) {
      return copy(connections.get(id));
    },
    async findActiveConnection(userId, clientId) {
      const active = await connectionStore.listActiveConnections(userId);
      return active.find((c) => c.clientId === clientId) ?? null;
    },
    async listActiveConnections(userId) {
      return [...connections.values()]
        .filter((c) => c.userId === userId && !c.revokedAt)
        .sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime())
        .map((c) => ({ ...c }));
    },
    async createConnection(record) {
      if (connections.has(record.id)) return null;
      const connection: ConnectionRecord = {
        ...record,
        refreshGeneration: 0,
        lastUsedAt: null,
        revokedAt: null,
      };
      connections.set(connection.id, connection);
      return { ...connection };
    },
    async updateConnection(id, patch) {
      const connection = connections.get(id);
      if (connection) Object.assign(connection, patch);
    },
    async advanceGeneration(id, from) {
      const connection = connections.get(id);
      if (
        !connection ||
        connection.revokedAt ||
        connection.refreshGeneration !== from
      )
        return false;
      connection.refreshGeneration = from + 1;
      return true;
    },
    async revokeConnection(id, now) {
      const connection = connections.get(id);
      if (connection && !connection.revokedAt) connection.revokedAt = now;
    },
  };

  const pendingActionStore: PendingActionStore = {
    async createPendingAction(record) {
      const action: PendingActionRecord = {
        ...record,
        id: randomUUID(),
        outcome: null,
        approvedAt: null,
        deniedAt: null,
        reportedAt: null,
        createdAt: new Date(),
      };
      pendingActions.set(action.id, action);
      return { ...action };
    },
    async getPendingAction(id) {
      const action = pendingActions.get(id);
      return action ? { ...action } : null;
    },
    async findOpenPendingAction(connectionId, toolName, argsKey, now) {
      const matches = [...pendingActions.values()]
        .filter(
          (a) =>
            a.connectionId === connectionId &&
            a.toolName === toolName &&
            a.argsKey === argsKey &&
            !a.reportedAt &&
            a.expiresAt > now,
        )
        .sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
      return matches[0] ? { ...matches[0] } : null;
    },
    async markPendingActionReported(id, now) {
      const action = pendingActions.get(id);
      if (action && !action.reportedAt) action.reportedAt = now;
    },
    async decidePendingAction(id, decision, now) {
      const action = pendingActions.get(id);
      if (!action || action.approvedAt || action.deniedAt) return false;
      if (decision === "approved") action.approvedAt = now;
      else action.deniedAt = now;
      return true;
    },
    async deleteExpiredPendingActions(before) {
      for (const [id, action] of pendingActions) {
        if (action.expiresAt < before) pendingActions.delete(id);
      }
    },
    async setPendingActionOutcome(id, outcome) {
      const action = pendingActions.get(id);
      if (action) action.outcome = outcome;
    },
  };

  return {
    connections: connectionStore,
    pendingActions: pendingActionStore,
    records: { connections, pendingActions },
  };
}
