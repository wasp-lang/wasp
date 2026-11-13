import type { RateLimitStore } from "./rateLimit";

// What the MCP server persists: only state that can't be stateless (§5.3).
// Codes and tokens are sealed and never stored (crypto.ts), and client
// metadata documents are cached in memory. A Wasp app backs this with the
// models Wasp adds to its Prisma schema (see prismaStore.ts); tests use the
// in-memory store.

/**
 * One authorization a user gave a client. A client installed twice (say on
 * two machines), or authorized again for more scopes, has one per
 * authorization; the connections page groups them by client.
 */
export type ConnectionRecord = {
  id: string;
  userId: string;
  /** The client's CIMD URL. */
  clientId: string;
  clientName: string;
  scopes: string[];
  /** The ceiling for later edits on the connections page. */
  requestedScopes: string[];
  requireConfirmation: boolean;
  /**
   * Bumped each time a code is exchanged or a refresh token is used. Codes
   * and refresh tokens carry the generation they expect, so each works once.
   */
  refreshGeneration: number;
  createdAt: Date;
  lastUsedAt: Date | null;
  revokedAt: Date | null;
};

export type PendingActionRecord = {
  id: string;
  /** The connection's user. Prisma stores read it from the connection. */
  userId: string;
  connectionId: string;
  toolName: string;
  args: unknown;
  /** SHA-256 of the args' canonical JSON: finds an identical earlier call. */
  argsKey: string;
  summary: { title: string; details?: string };
  outcome: unknown;
  expiresAt: Date;
  approvedAt: Date | null;
  deniedAt: Date | null;
  /** When the agent was told the outcome. Each outcome is reported once. */
  reportedAt: Date | null;
  createdAt: Date;
};

type New<T extends { id: string }> = Omit<T, "id">;

export interface ConnectionStore {
  getConnection(id: string): Promise<ConnectionRecord | null>;
  /** The newest unrevoked connection from this user to this client. */
  findActiveConnection(
    userId: string,
    clientId: string,
  ): Promise<ConnectionRecord | null>;
  /** Newest first. */
  listActiveConnections(userId: string): Promise<ConnectionRecord[]>;
  /**
   * Null if a connection with this id already exists: the id comes from the
   * authorization code, so that means the code was used twice.
   */
  createConnection(
    record: Omit<
      ConnectionRecord,
      "lastUsedAt" | "revokedAt" | "refreshGeneration"
    >,
  ): Promise<ConnectionRecord | null>;
  updateConnection(
    id: string,
    patch: Partial<
      Pick<
        ConnectionRecord,
        | "clientName"
        | "scopes"
        | "requestedScopes"
        | "requireConfirmation"
        | "lastUsedAt"
      >
    >,
  ): Promise<void>;
  /**
   * Moves `refreshGeneration` from `from` to `from + 1` if it's still `from`
   * and the connection isn't revoked. False otherwise: a code or refresh token
   * was used twice.
   */
  advanceGeneration(id: string, from: number): Promise<boolean>;
  /** Every code and token issued for it stops working on its next use. */
  revokeConnection(id: string, now: Date): Promise<void>;
}

export interface PendingActionStore {
  createPendingAction(
    record: Omit<
      New<PendingActionRecord>,
      "outcome" | "approvedAt" | "deniedAt" | "reportedAt" | "createdAt"
    >,
  ): Promise<PendingActionRecord>;
  getPendingAction(id: string): Promise<PendingActionRecord | null>;
  /** The newest unexpired action for the same call whose outcome the agent hasn't heard yet, if any. */
  findOpenPendingAction(
    connectionId: string,
    toolName: string,
    argsKey: string,
    now: Date,
  ): Promise<PendingActionRecord | null>;
  markPendingActionReported(id: string, now: Date): Promise<void>;
  /**
   * Records the decision if the action is still undecided. False if it was
   * already approved or denied: a double click can't run a tool twice.
   */
  decidePendingAction(
    id: string,
    decision: "approved" | "denied",
    now: Date,
  ): Promise<boolean>;
  setPendingActionOutcome(id: string, outcome: unknown): Promise<void>;
  /** Deletes every action that expired before `before`. */
  deleteExpiredPendingActions(before: Date): Promise<void>;
}

export type McpStore = {
  connections: ConnectionStore;
  pendingActions: PendingActionStore;
  /** Shares rate-limit counts across server instances. Defaults to counting in memory. */
  rateLimits?: RateLimitStore;
};
