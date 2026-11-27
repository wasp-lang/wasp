// An McpStore backed by the McpConnection, McpPendingAction and McpRateLimit
// models Wasp adds to the app's Prisma schema (waspc's DbGenerator.Mcp).
//
// The models work on every database Wasp supports: lists and JSON are stored
// as text, because Prisma has no scalar lists or JSON columns on SQLite.
import type { RateLimitStore } from "./rateLimit";
import type {
  ConnectionStore,
  McpStore,
  PendingActionRecord,
  PendingActionStore,
} from "./store";

/** A Prisma model delegate. The generated client's types differ per app, so they're loose here. */
export type McpPrismaDelegate = {
  findUnique(args: unknown): Promise<any>;
  findFirst(args: unknown): Promise<any>;
  findMany(args: unknown): Promise<any[]>;
  create(args: unknown): Promise<any>;
  update(args: unknown): Promise<any>;
  updateMany(args: unknown): Promise<{ count: number }>;
  deleteMany(args: unknown): Promise<{ count: number }>;
  upsert(args: unknown): Promise<any>;
};

/** The app's Prisma client: only the MCP models are used. */
export type McpPrismaClient = {
  mcpConnection: McpPrismaDelegate;
  mcpPendingAction: McpPrismaDelegate;
  mcpRateLimit: McpPrismaDelegate;
};

export type McpPrismaStoreOptions = {
  /**
   * Turns the lib's string user IDs into the type of `User.id`. Defaults to
   * leaving them as strings; pass `Number` for `Int` IDs.
   */
  userIdToDb?: (userId: string) => unknown;
};

const toList = (value: string): string[] => JSON.parse(value);
const fromList = (values: string[]): string => JSON.stringify(values);

const connectionFromRow = (row: any) =>
  row && {
    ...row,
    userId: String(row.userId),
    scopes: toList(row.scopes),
    requestedScopes: toList(row.requestedScopes),
  };

// Pending actions get their user from the connection.
const withUserId = { connection: { select: { userId: true } } };

const pendingActionFromRow = (row: any): PendingActionRecord | null => {
  if (!row) return null;
  const { connection, ...rest } = row;
  return {
    ...rest,
    userId: String(connection.userId),
    args: JSON.parse(row.args),
    summary: JSON.parse(row.summary),
    outcome: row.outcome === null ? null : JSON.parse(row.outcome),
  };
};

const pendingFields = ({
  userId: _userId,
  ...rest
}: Omit<
  PendingActionRecord,
  "id" | "outcome" | "approvedAt" | "deniedAt" | "reportedAt" | "createdAt"
>) => rest;

/**
 * Rate-limit windows in a table: one row per connection with its window's
 * start and count. Increments are atomic; two instances starting a new window
 * at the same moment can undercount by one, which a limit tolerates.
 */
function createPrismaRateLimitStore(model: McpPrismaDelegate): RateLimitStore {
  return {
    async hit(key, windowMs, now) {
      const { count } = await model.updateMany({
        where: {
          id: key,
          windowStart: { gt: new Date(now.getTime() - windowMs) },
        },
        data: { count: { increment: 1 } },
      });
      if (count === 1) {
        const row = await model.findUnique({ where: { id: key } });
        if (row) return { windowStart: row.windowStart, count: row.count };
      }
      const fresh = { windowStart: now, count: 1 };
      await model.upsert({
        where: { id: key },
        create: { id: key, ...fresh },
        update: fresh,
      });
      return fresh;
    },
  };
}

/** Prisma's error for a unique constraint violation. */
const isUniqueViolation = (e: unknown) =>
  typeof e === "object" &&
  e !== null &&
  (e as { code?: unknown }).code === "P2002";

export function createPrismaStore(
  prisma: McpPrismaClient,
  options: McpPrismaStoreOptions = {},
): McpStore {
  const connection = prisma.mcpConnection;
  const pendingAction = prisma.mcpPendingAction;
  const userIdToDb = options.userIdToDb ?? ((id: string) => id);

  const connections: ConnectionStore = {
    async getConnection(id) {
      return connectionFromRow(await connection.findUnique({ where: { id } }));
    },
    async findActiveConnection(userId, clientId) {
      return connectionFromRow(
        await connection.findFirst({
          where: { userId: userIdToDb(userId), clientId, revokedAt: null },
          orderBy: { createdAt: "desc" },
        }),
      );
    },
    async listActiveConnections(userId) {
      const rows = await connection.findMany({
        where: { userId: userIdToDb(userId), revokedAt: null },
        orderBy: { createdAt: "desc" },
      });
      return rows.map(connectionFromRow);
    },
    async createConnection(record) {
      try {
        return connectionFromRow(
          await connection.create({
            data: {
              ...record,
              userId: userIdToDb(record.userId),
              scopes: fromList(record.scopes),
              requestedScopes: fromList(record.requestedScopes),
            },
          }),
        );
      } catch (e) {
        if (isUniqueViolation(e)) return null;
        throw e;
      }
    },
    async updateConnection(id, patch) {
      const { scopes, requestedScopes, ...rest } = patch;
      await connection.update({
        where: { id },
        data: {
          ...rest,
          ...(scopes && { scopes: fromList(scopes) }),
          ...(requestedScopes && {
            requestedScopes: fromList(requestedScopes),
          }),
        },
      });
    },
    async advanceGeneration(id, from) {
      const { count } = await connection.updateMany({
        where: { id, refreshGeneration: from, revokedAt: null },
        data: { refreshGeneration: from + 1 },
      });
      return count === 1;
    },
    async revokeConnection(id, now) {
      await connection.updateMany({
        where: { id, revokedAt: null },
        data: { revokedAt: now },
      });
    },
  };

  const pendingActions: PendingActionStore = {
    async createPendingAction(record) {
      const created = pendingActionFromRow(
        await pendingAction.create({
          include: withUserId,
          data: {
            ...pendingFields(record),
            args: JSON.stringify(record.args),
            summary: JSON.stringify(record.summary),
          },
        }),
      );
      return created!;
    },
    async getPendingAction(id) {
      return pendingActionFromRow(
        await pendingAction.findUnique({
          where: { id },
          include: withUserId,
        }),
      );
    },
    async findOpenPendingAction(connectionId, toolName, argsKey, now) {
      return pendingActionFromRow(
        await pendingAction.findFirst({
          where: {
            connectionId,
            toolName,
            argsKey,
            reportedAt: null,
            expiresAt: { gt: now },
          },
          orderBy: { createdAt: "desc" },
          include: withUserId,
        }),
      );
    },
    async markPendingActionReported(id, now) {
      await pendingAction.updateMany({
        where: { id, reportedAt: null },
        data: { reportedAt: now },
      });
    },
    async decidePendingAction(id, decision, now) {
      const { count } = await pendingAction.updateMany({
        where: { id, approvedAt: null, deniedAt: null },
        data: decision === "approved" ? { approvedAt: now } : { deniedAt: now },
      });
      return count === 1;
    },
    async deleteExpiredPendingActions(before) {
      await pendingAction.deleteMany({ where: { expiresAt: { lt: before } } });
    },
    async setPendingActionOutcome(id, outcome) {
      await pendingAction.update({
        where: { id },
        data: { outcome: JSON.stringify(outcome) },
      });
    },
  };

  return {
    connections,
    pendingActions,
    rateLimits: createPrismaRateLimitStore(prisma.mcpRateLimit),
  };
}
