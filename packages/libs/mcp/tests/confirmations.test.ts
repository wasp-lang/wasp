import { afterEach, describe, expect, it } from "vitest";
import { z } from "zod";
import { createMemoryStore, defineMcpTool } from "../src/node";
import { createTestApp, FRONTEND_URL, type TestApp } from "./helpers";

let open: { close: () => Promise<void> }[] = [];
afterEach(async () => {
  for (const c of open) await c.close();
  open = [];
});

const URL_MODE = { elicitation: { url: {} } };

function setup() {
  const app = createTestApp();
  app.notes.push({ id: "n1", userId: "u1", text: "buy milk" });
  return { app };
}

async function connect(
  app: TestApp,
  opts: Parameters<TestApp["connect"]>[1] & {
    requireConfirmation?: boolean;
  } = {},
) {
  const { access_token } = await app.authorize("u1", {
    requireConfirmation: opts.requireConfirmation ?? true,
  });
  const conn = await app.connect(access_token, opts);
  open.push(conn);
  return { ...conn, access_token };
}

const structured = (r: { structuredContent?: unknown }) =>
  r.structuredContent as Record<string, any>;
const actionIdFrom = (url: string) => new URL(url).searchParams.get("action")!;

describe("approval", () => {
  it("stores an error outcome when the result can't be saved, so the client hears back", async () => {
    const app = createTestApp({
      tools: [
        defineMcpTool({
          name: "wipe",
          description: "Can't be undone.",
          input: z.object({}),
          destructive: true,
          operation: async () => ({ count: 10n }),
        }),
      ],
    });
    const { call } = await connect(app);
    const { actionId } = structured(await call("wipe"));
    const view = await app.mcp.user.approvePendingAction("u1", { actionId });
    expect(view.outcome).toMatchObject({
      ok: false,
      error: { message: "It ran, but its result couldn't be saved." },
    });
    // Checking back reports it as a tool error.
    const report = await call("wipe");
    expect(report.isError).toBe(true);
    expect(JSON.stringify(report.content)).toContain("couldn't be saved");
  });
});

describe("without confirmation", () => {
  it("runs right away when the connection turned confirmations off", async () => {
    const { app } = setup();
    const { call } = await connect(app, { requireConfirmation: false });
    expect(structured(await call("delete_note", { id: "n1" }))).toEqual({
      status: "completed",
      result: { id: "n1" },
    });
    expect(app.notes).toEqual([]);
  });
});

describe("the confirm link (any client)", () => {
  it("returns a link, runs the tool when the user approves, and reports the result once", async () => {
    const { app } = setup();
    const { call } = await connect(app);

    const first = structured(await call("delete_note", { id: "n1" }));
    expect(first).toMatchObject({
      status: "confirmation_required",
      actionId: expect.any(String),
    });
    expect(first.confirmUrl).toBe(
      `${FRONTEND_URL}/mcp/confirm?action=${first.actionId}`,
    );
    expect(app.notes).toHaveLength(1);

    // The confirm page.
    const view = await app.mcp.user.getPendingAction("u1", {
      actionId: first.actionId,
    });
    expect(view).toMatchObject({
      status: "pending",
      toolName: "delete_note",
      toolTitle: "Delete a note",
      summary: { title: "Delete note n1", details: "buy milk" },
      args: { id: "n1" },
      clientName: "Test Client",
      clientHost: "client.example",
    });

    const approved = await app.mcp.user.approvePendingAction("u1", {
      actionId: first.actionId,
    });
    expect(approved).toMatchObject({
      status: "approved",
      outcome: { ok: true, result: { id: "n1" } },
    });
    expect(app.notes).toEqual([]);

    // The agent checks back with the same call: it hears the outcome, once.
    expect(structured(await call("delete_note", { id: "n1" }))).toMatchObject({
      status: "completed",
      result: { id: "n1" },
      actionId: first.actionId,
    });
    const third = structured(await call("delete_note", { id: "n1" }));
    expect(third.status).toBe("confirmation_required");
    expect(third.actionId).not.toBe(first.actionId);
  });

  it("hands back the same link for the same call while it's pending", async () => {
    const { app } = setup();
    const { call } = await connect(app);
    const a = structured(await call("delete_note", { id: "n1" }));
    const b = structured(await call("delete_note", { id: "n1" }));
    expect(b.actionId).toBe(a.actionId);
  });

  it("tells the agent once that the user denied it", async () => {
    const { app } = setup();
    const { call } = await connect(app);
    const { actionId } = structured(await call("delete_note", { id: "n1" }));
    await app.mcp.user.denyPendingAction("u1", { actionId });
    expect(structured(await call("delete_note", { id: "n1" }))).toMatchObject({
      status: "denied",
      actionId,
    });
    expect(app.notes).toHaveLength(1);
  });

  it("reports the tool's error if it fails after approval", async () => {
    const { app } = setup();
    const { call } = await connect(app);
    const { actionId } = structured(
      await call("delete_note", { id: "missing" }),
    );
    const view = await app.mcp.user.approvePendingAction("u1", { actionId });
    expect(view.outcome).toMatchObject({
      ok: false,
      error: { code: "not_found" },
    });
    const result = await call("delete_note", { id: "missing" });
    expect(result.isError).toBe(true);
    expect(result.content).toEqual([
      {
        type: "text",
        text: "not_found: Note missing not found. Use list_notes to find valid IDs.",
      },
    ]);
  });

  it("can only be approved once, by its own user", async () => {
    const { app } = setup();
    const { call } = await connect(app);
    const { actionId } = structured(await call("delete_note", { id: "n1" }));
    await expect(
      app.mcp.user.getPendingAction("u2", { actionId }),
    ).rejects.toThrow(/isn't valid for your account/);
    await expect(
      app.mcp.user.approvePendingAction("u2", { actionId }),
    ).rejects.toThrow(/isn't valid for your account/);
    await app.mcp.user.approvePendingAction("u1", { actionId });
    await expect(
      app.mcp.user.approvePendingAction("u1", { actionId }),
    ).rejects.toThrow(/approved and can't be approved/);
  });

  it("doesn't run if the connection was revoked before approval", async () => {
    const { app } = setup();
    const { call } = await connect(app);
    const { actionId } = structured(await call("delete_note", { id: "n1" }));
    const [connection] = await app.mcp.user.listConnections("u1");
    await app.mcp.user.revokeConnection("u1", {
      clientId: connection!.clientId,
    });
    const view = await app.mcp.user.approvePendingAction("u1", { actionId });
    expect(view.outcome).toMatchObject({
      ok: false,
      error: { code: "unauthorized" },
    });
    expect(app.notes).toHaveLength(1);
  });
});

describe("URL-mode elicitation", () => {
  type Decision = "accept" | "decline" | "cancel";

  async function urlClient(
    app: TestApp,
    decide: (actionId: string) => Decision | Promise<Decision>,
  ) {
    const prompts: { mode?: string; url?: string; message: string }[] = [];
    const conn = await connect(app, {
      capabilities: URL_MODE,
      onElicit: async (params) => {
        const p = params as { mode?: string; url: string; message: string };
        prompts.push(p);
        return { action: await decide(actionIdFrom(p.url)) };
      },
    });
    return { ...conn, prompts };
  }

  it("opens the confirm page, and returns the result once the user approves", async () => {
    const { app } = setup();
    const c = await urlClient(app, async (actionId): Promise<"accept"> => {
      expect(app.notes).toHaveLength(1); // nothing happened before the user approved
      await app.mcp.user.approvePendingAction("u1", { actionId });
      return "accept";
    });
    expect(structured(await c.call("delete_note", { id: "n1" }))).toMatchObject(
      { status: "completed", result: { id: "n1" } },
    );
    expect(c.prompts).toEqual([
      {
        mode: "url",
        url: expect.stringMatching(/\/mcp\/confirm\?action=/),
        message: "Approve in Notes: Delete note n1",
      },
    ]);
    expect(app.notes).toEqual([]);
  });

  it("waits for a decision made after the page opened", async () => {
    const { app } = setup();
    const c = await urlClient(app, (actionId) => {
      setTimeout(
        () => void app.mcp.user.approvePendingAction("u1", { actionId }),
        50,
      );
      return "accept";
    });
    expect(structured(await c.call("delete_note", { id: "n1" }))).toMatchObject(
      { status: "completed" },
    );
    expect(c.prompts).toHaveLength(1); // the user was asked once
  });

  it("withdraws the request when the user declines to open the page", async () => {
    const { app } = setup();
    const c = await urlClient(app, () => "decline");
    expect(structured(await c.call("delete_note", { id: "n1" }))).toMatchObject(
      { status: "denied" },
    );
    expect(app.notes).toHaveLength(1);
  });

  it("falls back to the link when the user doesn't decide in time", async () => {
    const { app } = setup();
    const c = await urlClient(app, () => "accept");
    const result = structured(await c.call("delete_note", { id: "n1" }));
    expect(result).toMatchObject({
      status: "confirmation_required",
      confirmUrl: expect.stringContaining("/mcp/confirm?action="),
    });
    expect(app.notes).toHaveLength(1);
  }, 10_000);
});

describe("with elicitation turned off", () => {
  it("gives the link even to clients that support URL mode", async () => {
    const app = createTestApp({ confirmation: { elicitation: false } });
    app.notes.push({ id: "n1", userId: "u1", text: "buy milk" });
    let prompted = false;
    const { call } = await connect(app, {
      capabilities: URL_MODE,
      onElicit: async () => {
        prompted = true;
        return { action: "accept" };
      },
    });
    expect(structured(await call("delete_note", { id: "n1" }))).toMatchObject({
      status: "confirmation_required",
      confirmUrl: expect.stringContaining("/mcp/confirm?action="),
    });
    expect(prompted).toBe(false);
  });
});

describe("cleanup", () => {
  it("drops actions a day after they expire, when any connection makes a new one", async () => {
    let now = new Date("2026-11-12T10:00:00Z");
    const store = createMemoryStore();
    const app = createTestApp({ store, now: () => now });
    app.notes.push(
      { id: "n1", userId: "u1", text: "a" },
      { id: "n2", userId: "u1", text: "b" },
    );
    const { call } = await connect(app);
    await call("delete_note", { id: "n1" });
    expect(store.records.pendingActions.size).toBe(1);

    now = new Date("2026-11-13T12:00:00Z"); // past expiry + retention
    // A day later the access token has expired too, so the client connects
    // again: a new connection, which never sees the old action.
    const later = await connect(app);
    await later.call("delete_note", { id: "n2" });
    await new Promise((r) => setTimeout(r, 10)); // cleanup is fire and forget
    expect(
      [...store.records.pendingActions.values()].map((a) => a.args),
    ).toEqual([{ id: "n2" }]);
  });
});
