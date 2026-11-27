// Destructive tools wait for the human. A call becomes a pending action the
// user approves on the app's confirm page, in their own session. Approving
// runs the tool right there, once, and stores the outcome; the agent's retry
// only reads it.
//
// Clients that support URL-mode elicitation (MCP 2026-07-28) open the page
// themselves, and the retried call waits a little for the decision. Every
// other client gets the page as `confirmUrl` in the result. The client's
// "accept" only means the user agreed to open the page: the outcome always
// comes from the pending action, never from the client.
import {
  CLIENT_CAPABILITIES_META_KEY,
  createRequestStateCodec,
  inputRequired,
  inputResponse,
  type ClientCapabilities,
  type InputRequiredResult,
  type ServerContext,
} from "@modelcontextprotocol/server";
import type {
  JsonValue,
  PendingActionStatus,
  PendingActionView,
  ToolErrorView,
} from "../views";
import { principalFromConnection } from "./authenticate";
import type { AnyMcpTool, McpDescribeResult, McpPrincipal } from "./config";
import { canonicalJson, sha256Hex } from "./crypto";
import { McpToolError, McpUserError, toToolError } from "./errors";
import type { Runtime } from "./runtime";
import type { PendingActionRecord } from "./store";
import { hasScope, operationContext, runTool } from "./tools";

const PENDING_TTL_MS = 15 * 60 * 1000;

/** What the client echoes back on each retry. Signed, not encrypted: no secrets in it. */
type ConfirmationState = { actionId: string; tool: string; waits: number };

type Outcome =
  | { ok: true; result: unknown }
  | { ok: false; error: ToolErrorView };

export type ConfirmationEnvelope = {
  status: "completed" | "confirmation_required" | "denied" | "expired";
  result?: unknown;
  confirmUrl?: string;
  actionId?: string;
  expiresAt?: string;
  message?: string;
};

/** A tool error the agent should see as an error, not as a status. */
export type ConfirmationError = { error: ToolErrorView };

const DEFAULTS = {
  elicitation: true,
  waitMs: 20_000,
  pollMs: 1_000,
  maxWaits: 8,
};

/** Pending actions are kept a day past expiry, so a late check-back still learns what happened. */
const PENDING_RETENTION_MS = 24 * 60 * 60 * 1000;

/** Capabilities travel with every 2026-07-28 request; 2025-era requests carry none, so they get the link. */
function canOpenUrls(ctx: ServerContext): boolean {
  const envelope = ctx.mcpReq.envelope as Record<string, unknown> | undefined;
  const capabilities = envelope?.[CLIENT_CAPABILITIES_META_KEY] as
    | ClientCapabilities
    | undefined;
  return capabilities?.elicitation?.url !== undefined;
}

const sleep = (ms: number, signal: AbortSignal) =>
  new Promise<void>((resolve) => {
    const timer = setTimeout(resolve, ms);
    signal.addEventListener("abort", () => (clearTimeout(timer), resolve()), {
      once: true,
    });
  });

function statusOf(action: PendingActionRecord, now: Date): PendingActionStatus {
  if (action.approvedAt) return "approved";
  if (action.deniedAt) return "denied";
  if (action.expiresAt <= now) return "expired";
  return "pending";
}

/** Settled: the agent can be told the outcome. An approved action settles once the tool has run. */
const isSettled = (action: PendingActionRecord, now: Date) => {
  const status = statusOf(action, now);
  return (
    status === "denied" ||
    status === "expired" ||
    (status === "approved" && action.outcome !== null)
  );
};

export function createConfirmations<User>(rt: Runtime<User>) {
  const { elicitation, waitMs, pollMs, maxWaits } = {
    ...DEFAULTS,
    ...rt.config.confirmation,
  };

  const codecFor = (principal: McpPrincipal) =>
    createRequestStateCodec<ConfirmationState>({
      key: rt.keys.requestState,
      ttlSeconds: PENDING_TTL_MS / 1000,
      // Bound to the connection, so another connection can't replay it.
      bind: () => `${principal.userId}\0${principal.connectionId}`,
    });

  async function describe(
    tool: AnyMcpTool<User>,
    args: unknown,
    principal: McpPrincipal,
  ): Promise<McpDescribeResult> {
    const custom =
      typeof tool.destructive === "object"
        ? tool.destructive.describe
        : undefined;
    if (custom)
      return custom(args as never, await operationContext(rt, principal));
    return {
      title: tool.title ?? tool.name,
      details: JSON.stringify(args, null, 2),
    };
  }

  const pendingEnvelope = (
    action: PendingActionRecord,
    message: string,
  ): ConfirmationEnvelope => ({
    status: "confirmation_required",
    confirmUrl: rt.urls.confirmUrl(action.id),
    actionId: action.id,
    expiresAt: action.expiresAt.toISOString(),
    message,
  });

  /** The agent's view of a settled action. */
  function settledEnvelope(
    action: PendingActionRecord,
  ): ConfirmationEnvelope | ConfirmationError {
    switch (statusOf(action, rt.now())) {
      case "approved": {
        const outcome = action.outcome as Outcome;
        if (!outcome.ok) return { error: outcome.error };
        return {
          status: "completed",
          result: outcome.result,
          actionId: action.id,
          message: "The user approved this in the app, and it's done.",
        };
      }
      case "denied":
        return {
          status: "denied",
          actionId: action.id,
          message: "The user denied this. Nothing changed.",
        };
      case "expired":
      default:
        return {
          status: "expired",
          actionId: action.id,
          message:
            "The approval expired before the user decided. Nothing changed; call the tool again if it's still wanted.",
        };
    }
  }

  async function findForPrincipal(
    principal: McpPrincipal,
    actionId: string,
  ): Promise<PendingActionRecord> {
    const action = await rt.pendingActions.getPendingAction(actionId);
    if (
      !action ||
      action.userId !== principal.userId ||
      action.connectionId !== principal.connectionId
    ) {
      throw new McpToolError(
        "not_found",
        "That pending action doesn't exist.",
        "Call the tool again.",
      );
    }
    return action;
  }

  /** Polls until the action settles, the wait runs out, or the client goes away. */
  async function waitForDecision(
    action: PendingActionRecord,
    signal: AbortSignal,
  ): Promise<PendingActionRecord> {
    const until = Date.now() + waitMs;
    while (
      !isSettled(action, rt.now()) &&
      Date.now() < until &&
      !signal.aborted
    ) {
      await sleep(pollMs, signal);
      action = (await rt.pendingActions.getPendingAction(action.id)) ?? action;
    }
    return action;
  }

  return {
    /** For `ServerOptions.requestState.verify`: tampered, expired, or another connection's state never reaches a handler. */
    verifierFor: (principal: McpPrincipal) => codecFor(principal).verify,

    /** First call: record the pending action, then ask the client to open the page, or hand back the link. */
    async request(
      tool: AnyMcpTool<User>,
      rawArgs: unknown,
      principal: McpPrincipal,
      ctx: ServerContext,
    ): Promise<ConfirmationEnvelope | ConfirmationError | InputRequiredResult> {
      const args = tool.input.parse(rawArgs ?? {});
      const argsKey = sha256Hex(canonicalJson(args));

      // The same call again (an agent without URL mode checking back): report
      // the outcome once, or hand back the same link instead of a second request.
      const existing = await rt.pendingActions.findOpenPendingAction(
        principal.connectionId,
        tool.name,
        argsKey,
        rt.now(),
      );
      if (existing && isSettled(existing, rt.now())) {
        await rt.pendingActions.markPendingActionReported(
          existing.id,
          rt.now(),
        );
        return settledEnvelope(existing);
      }

      const action =
        existing ??
        (await rt.pendingActions.createPendingAction({
          userId: principal.userId,
          connectionId: principal.connectionId,
          toolName: tool.name,
          args,
          argsKey,
          summary: await describe(tool, args, principal),
          expiresAt: new Date(rt.now().getTime() + PENDING_TTL_MS),
        }));
      if (!existing) {
        // Housekeeping: drop long-expired actions, of any connection (one that
        // was reauthorized never makes another). Fire and forget, so it never
        // slows down or fails the call.
        rt.pendingActions
          .deleteExpiredPendingActions(
            new Date(rt.now().getTime() - PENDING_RETENTION_MS),
          )
          .catch(() => {});
      }
      const summary = action.summary;
      if (elicitation && canOpenUrls(ctx)) {
        return inputRequired({
          inputRequests: {
            confirm: inputRequired.elicitUrl({
              url: rt.urls.confirmUrl(action.id),
              message: `Approve in ${rt.config.title ?? rt.config.name}: ${summary.title}`,
            }),
          },
          requestState: await codecFor(principal).mint(
            { actionId: action.id, tool: tool.name, waits: 0 },
            ctx,
          ),
        });
      }
      return pendingEnvelope(
        action,
        "Nothing has happened yet. Share confirmUrl with the user; this runs once they approve it.",
      );
    },

    /** A retry carrying requestState: report what the user decided. Undefined for a first call. */
    async resume(
      tool: AnyMcpTool<User>,
      principal: McpPrincipal,
      ctx: ServerContext,
    ): Promise<
      ConfirmationEnvelope | ConfirmationError | InputRequiredResult | undefined
    > {
      const state = ctx.mcpReq.requestState<ConfirmationState>();
      if (!state) return undefined;
      if (state.tool !== tool.name) {
        throw new McpToolError(
          "invalid_input",
          "This retry doesn't match the call that started it.",
          "Call the tool again.",
        );
      }
      let action = await findForPrincipal(principal, state.actionId);
      const answer = inputResponse(ctx.mcpReq.inputResponses, "confirm");
      const pending = statusOf(action, rt.now()) === "pending";
      if (pending && answer.kind === "elicit" && answer.action === "decline") {
        // The user declined to open the page: withdraw the request so its link can't be used later.
        await rt.pendingActions.decidePendingAction(
          action.id,
          "denied",
          rt.now(),
        );
        action =
          (await rt.pendingActions.getPendingAction(action.id)) ?? action;
      } else if (
        pending &&
        answer.kind === "elicit" &&
        answer.action === "cancel"
      ) {
        return pendingEnvelope(
          action,
          "The user dismissed the prompt, so nothing has happened yet. Share confirmUrl with the user; this runs once they approve it.",
        );
      }

      action = await waitForDecision(action, ctx.mcpReq.signal);
      if (isSettled(action, rt.now())) {
        await rt.pendingActions.markPendingActionReported(action.id, rt.now());
        return settledEnvelope(action);
      }
      if (state.waits + 1 >= maxWaits) {
        return pendingEnvelope(
          action,
          "The user hasn't decided yet, so nothing has happened. Share confirmUrl with the user; this runs once they approve it.",
        );
      }
      // State only: the client retries without asking the user again.
      return inputRequired({
        requestState: await codecFor(principal).mint(
          { ...state, waits: state.waits + 1 },
          ctx,
        ),
      });
    },

    // The confirm page. Each takes the logged-in user's id.

    async getPendingAction(
      userId: string,
      actionId: string,
    ): Promise<PendingActionView> {
      return toView(await findForUser(userId, actionId));
    },

    async approvePendingAction(
      userId: string,
      actionId: string,
    ): Promise<PendingActionView> {
      const action = await findForUser(userId, actionId);
      const status = statusOf(action, rt.now());
      if (status !== "pending")
        throw new McpUserError(
          409,
          `This request is ${status} and can't be approved.`,
        );
      if (
        !(await rt.pendingActions.decidePendingAction(
          action.id,
          "approved",
          rt.now(),
        ))
      ) {
        throw new McpUserError(409, "This request was already handled.");
      }
      // The action is approved now: whatever happens, it gets an outcome, or
      // the client would wait for one until the action expired.
      const outcome = asStorable(
        await execute(action).catch(
          (e): Outcome => ({ ok: false, error: toToolError(e) }),
        ),
      );
      await rt.pendingActions.setPendingActionOutcome(action.id, outcome);
      return toView({ ...action, approvedAt: rt.now(), outcome });
    },

    async denyPendingAction(
      userId: string,
      actionId: string,
    ): Promise<PendingActionView> {
      const action = await findForUser(userId, actionId);
      const status = statusOf(action, rt.now());
      if (status !== "pending")
        throw new McpUserError(
          409,
          `This request is ${status} and can't be denied.`,
        );
      if (
        !(await rt.pendingActions.decidePendingAction(
          action.id,
          "denied",
          rt.now(),
        ))
      ) {
        throw new McpUserError(409, "This request was already handled.");
      }
      return toView({ ...action, deniedAt: rt.now() });
    },
  };

  /** Other users' actions are "not found". */
  async function findForUser(
    userId: string,
    actionId: string,
  ): Promise<PendingActionRecord> {
    const action = await rt.pendingActions.getPendingAction(actionId);
    if (!action || action.userId !== userId)
      throw new McpUserError(
        404,
        "This approval link isn't valid for your account.",
      );
    return action;
  }

  /** Runs the approved tool as the connection, which is checked again now: it may have been revoked or narrowed. */
  /** The outcome as plain JSON, as the store saves it. A result JSON can't hold (a BigInt, say) becomes an error. */
  function asStorable(outcome: Outcome): Outcome {
    try {
      return JSON.parse(JSON.stringify(outcome)) as Outcome;
    } catch {
      return {
        ok: false,
        error: {
          code: "internal",
          message: "It ran, but its result couldn't be saved.",
        },
      };
    }
  }

  async function execute(action: PendingActionRecord): Promise<Outcome> {
    const tool = rt.tools.get(action.toolName);
    const connection = await rt.connections.getConnection(action.connectionId);
    if (!tool)
      return {
        ok: false,
        error: {
          code: "not_found",
          message: `The tool ${action.toolName} no longer exists.`,
        },
      };
    if (!connection || connection.revokedAt) {
      return {
        ok: false,
        error: {
          code: "unauthorized",
          message: "The connection that asked for this was revoked.",
        },
      };
    }
    const principal = principalFromConnection(connection);
    if (!hasScope(rt, principal, tool.scope)) {
      return {
        ok: false,
        error: {
          code: "forbidden",
          message: `The connection no longer has the "${tool.scope}" permission.`,
        },
      };
    }
    try {
      return {
        ok: true,
        result: await runTool(rt, tool, action.args, principal),
      };
    } catch (e) {
      return { ok: false, error: toToolError(e) };
    }
  }

  async function toView(
    action: PendingActionRecord,
  ): Promise<PendingActionView> {
    const connection = await rt.connections.getConnection(action.connectionId);
    const tool = rt.tools.get(action.toolName);
    let clientHost = "";
    try {
      clientHost = connection ? new URL(connection.clientId).hostname : "";
    } catch {
      clientHost = connection?.clientId ?? "";
    }
    return {
      id: action.id,
      status: statusOf(action, rt.now()),
      toolName: action.toolName,
      toolTitle: tool?.title ?? action.toolName,
      summary: action.summary,
      // Args and results went through JSON when they were stored.
      args: action.args as JsonValue,
      clientName: connection?.clientName ?? "An app",
      clientHost,
      expiresAt: action.expiresAt.toISOString(),
      outcome: (action.outcome as PendingActionView["outcome"]) ?? null,
    };
  }
}

export type Confirmations = ReturnType<typeof createConfirmations<any>>;
