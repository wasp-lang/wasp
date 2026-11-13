import { afterEach, describe, expect, it } from "vitest";
import { z } from "zod";
import {
  createMemoryRateLimitStore,
  createMemoryStore,
  defineMcpTool,
} from "../src/node";
import { createTestApp, SERVER_URL, type TestApp } from "./helpers";

let open: { close: () => Promise<void> }[] = [];
afterEach(async () => {
  for (const c of open) await c.close();
  open = [];
});

async function connected(
  app: TestApp,
  opts: Parameters<TestApp["authorize"]>[1] = {},
) {
  const { access_token } = await app.authorize("u1", opts);
  const conn = await app.connect(access_token);
  open.push(conn);
  return { ...conn, access_token };
}

describe("tools", () => {
  it("advertises the server's name and instructions", async () => {
    const { client } = await connected(createTestApp());
    expect(client.getServerVersion()).toMatchObject({
      name: "notes",
      title: "Notes",
    });
    expect(client.getInstructions()).toBe(
      "Notes keeps short notes. Use list_notes first.",
    );
  });

  it("lists tools with schemas and annotations, plus whoami", async () => {
    const { client } = await connected(createTestApp());
    const { tools } = await client.listTools();
    const byName = Object.fromEntries(tools.map((t) => [t.name, t]));
    expect(Object.keys(byName).sort()).toEqual([
      "create_note",
      "delete_note",
      "explode",
      "list_notes",
      "whoami",
    ]);
    expect(byName.list_notes!.annotations).toMatchObject({
      readOnlyHint: true,
      destructiveHint: false,
    });
    expect(byName.delete_note!.annotations).toMatchObject({
      destructiveHint: true,
    });
    expect(byName.delete_note!.description).toContain("confirmation_required");
    expect(byName.delete_note!.outputSchema?.properties).toHaveProperty(
      "status",
    );
    expect(byName.create_note!.inputSchema.required).toEqual(["text"]);
  });

  it("runs tools as the connected user and returns structured content", async () => {
    const app = createTestApp();
    const { call } = await connected(app);
    const created = await call("create_note", { text: "buy milk" });
    // Output isn't filtered by its schema (D22): the model gets what the operation returns.
    expect(created.structuredContent).toEqual({
      id: "1",
      text: "buy milk",
      extra: "not in the output schema",
    });
    expect(app.notes).toEqual([{ id: "1", userId: "u1", text: "buy milk" }]);
    expect((await call("list_notes")).structuredContent).toEqual({
      notes: [{ id: "1", text: "buy milk" }],
    });
  });

  it("returns actions that give back nothing, and array outputs, in every era", async () => {
    const app = createTestApp({
      tools: [
        defineMcpTool({
          name: "touch",
          description: "Returns nothing.",
          input: z.object({}),
          operation: async () => undefined,
        }),
        defineMcpTool({
          name: "list_ids",
          description: "Returns an array.",
          input: z.object({}),
          output: z.array(z.string()),
          operation: async () => ["a", "b"],
        }),
      ],
    });
    const { access_token } = await app.authorize("u1");
    for (const era of ["modern", "legacy"] as const) {
      const conn = await app.connect(access_token, { era });
      open.push(conn);
      const touched = await conn.call("touch");
      expect(touched.isError).toBeFalsy();
      expect(touched.content).toEqual([{ type: "text", text: "null" }]);
      const listed = await conn.call("list_ids");
      expect(listed.isError).toBeFalsy();
      expect(JSON.stringify(listed.structuredContent)).toContain('"a"');
    }
  });

  it("calls the operation with the user and context.mcp", async () => {
    const seen: unknown[] = [];
    const app = createTestApp({
      tools: [
        defineMcpTool({
          name: "inspect",
          description: "Returns what the operation saw.",
          input: z.object({ n: z.number() }),
          operation: async (args, context) => {
            seen.push({ args, context });
            return { ok: true };
          },
        }),
      ],
    });
    const { call } = await connected(app, { scopes: ["notes:write"] });
    await call("inspect", { n: 1 });
    expect(seen).toEqual([
      {
        args: { n: 1 },
        context: {
          user: { id: "u1", username: "alice" },
          mcp: {
            // Implied scopes are expanded.
            scopes: ["notes:read", "notes:write"],
            connection: {
              id: expect.any(String),
              clientId: "https://client.example/oauth/metadata.json",
              clientName: "Test Client",
              requireConfirmation: true,
            },
          },
        },
      },
    ]);
  });

  it("can mark a tool destructive without the confirm flow", async () => {
    const app = createTestApp({
      tools: [
        defineMcpTool({
          name: "wipe",
          description: "Wipes.",
          input: z.object({}),
          destructiveHint: true,
          operation: async () => ({ wiped: true }),
        }),
      ],
    });
    const { client, call } = await connected(app);
    const { tools } = await client.listTools();
    const wipe = tools.find((t) => t.name === "wipe")!;
    expect(wipe.annotations?.destructiveHint).toBe(true);
    expect(wipe.description).toBe("Wipes.");
    // Runs right away: no confirmation envelope.
    expect((await call("wipe")).structuredContent).toEqual({ wiped: true });
  });

  it("refuses schemas that can't be sent as JSON, naming the tool", () => {
    const tool = (input: z.ZodObject, output?: z.ZodType) =>
      createTestApp({
        tools: [
          defineMcpTool({
            name: "bad",
            description: "Has a bad schema.",
            input,
            output,
            operation: async () => ({}),
          }),
        ],
      });
    expect(() => tool(z.object({ at: z.date() }))).toThrow(
      /MCP tool "bad": its input schema can't be sent as JSON \(Date cannot be represented/,
    );
    expect(() =>
      tool(z.object({}), z.object({ n: z.string().transform(Number) })),
    ).toThrow(/its output schema can't be sent as JSON/);
    expect(() => tool(z.object({ at: z.iso.datetime() }))).not.toThrow();
  });

  it("whoami reports the user, scopes, and confirmation setting", async () => {
    const { call } = await connected(createTestApp(), {
      scopes: ["notes:read"],
      requireConfirmation: false,
    });
    expect((await call("whoami")).structuredContent).toEqual({
      user: { id: "u1", username: "alice" },
      scopes: ["notes:read"],
      requireConfirmation: false,
      client: {
        id: "https://client.example/oauth/metadata.json",
        name: "Test Client",
      },
    });
  });

  it("can turn whoami off", async () => {
    const { toolNames } = await connected(createTestApp({ whoami: false }));
    expect(await toolNames()).not.toContain("whoami");
  });

  it("maps errors to codes with next steps, and hides internal details", async () => {
    const { call } = await connected(createTestApp(), {
      requireConfirmation: false,
    });
    const notFound = await call("delete_note", { id: "404" });
    expect(notFound.isError).toBe(true);
    expect(notFound.content).toEqual([
      {
        type: "text",
        text: "not_found: Note 404 not found. Use list_notes to find valid IDs.",
      },
    ]);

    expect((await call("explode", { kind: "tool" })).content).toEqual([
      { type: "text", text: "conflict: Already exploded. Try something else." },
    ]);
    const internal = await call("explode", { kind: "internal" });
    expect(JSON.stringify(internal.content)).not.toContain("hunter2");
    expect(JSON.stringify(internal.content)).toContain("internal:");
  });

  it("rejects invalid input before the handler runs", async () => {
    const app = createTestApp();
    const { call } = await connected(app);
    const result = await call("create_note", { text: "" });
    expect(result.isError).toBe(true);
    expect(app.notes).toEqual([]);
  });
});

describe("insufficient scope", () => {
  it("step-up (default): lists the tool, and answers a call with 403 insufficient_scope", async () => {
    const app = createTestApp();
    const { toolNames, access_token } = await connected(app, {
      scopes: ["notes:read"],
    });
    expect(await toolNames()).toContain("create_note");

    const res = await app.rawCall(access_token, "create_note", { text: "hi" });
    expect(res.status).toBe(403);
    const challenge = res.headers.get("WWW-Authenticate")!;
    expect(challenge).toContain('error="insufficient_scope"');
    // What's granted plus what's missing, so re-consent keeps the other tools.
    expect(challenge).toContain('scope="notes:read notes:write"');
    expect(challenge).toContain(
      `resource_metadata="${SERVER_URL}/.well-known/oauth-protected-resource/mcp"`,
    );
    expect(app.notes).toEqual([]);
  });

  it("hide: leaves the tool out of the list, and a call can't run it", async () => {
    const app = createTestApp({ insufficientScope: "hide" });
    const { toolNames, call } = await connected(app, {
      scopes: ["notes:read"],
    });
    expect(await toolNames()).toEqual(["explode", "list_notes", "whoami"]);
    const result = await call("create_note", { text: "hi" }).catch(
      (e: Error) => ({ isError: true, error: e.message }),
    );
    expect(result.isError).toBe(true);
    expect(app.notes).toEqual([]);
  });

  it("a tool can override the server's choice", async () => {
    const app = createTestApp({
      insufficientScope: "hide",
      tools: [
        defineMcpTool({
          name: "create_note",
          description: "Creates a note.",
          input: z.object({ text: z.string() }),
          scope: "notes:write",
          insufficientScope: "stepUp",
          operation: async () => ({}),
        }),
      ],
    });
    const { toolNames } = await connected(app, { scopes: ["notes:read"] });
    expect(await toolNames()).toEqual(["create_note", "whoami"]);
  });
});

describe("rate limiting", () => {
  it("answers 429 with Retry-After per connection", async () => {
    const app = createTestApp({ rateLimit: { windowMs: 60_000, max: 2 } });
    const { access_token } = await app.authorize("u1");
    expect((await app.rawCall(access_token, "list_notes")).status).toBe(200);
    expect((await app.rawCall(access_token, "list_notes")).status).toBe(200);
    const limited = await app.rawCall(access_token, "list_notes");
    expect(limited.status).toBe(429);
    expect(Number(limited.headers.get("Retry-After"))).toBeGreaterThan(0);

    // Another connection isn't affected.
    const other = await app.authorize("u2");
    expect((await app.rawCall(other.access_token, "list_notes")).status).toBe(
      200,
    );
  });

  it("shares the count across server instances through a shared store", async () => {
    const store = {
      ...createMemoryStore(),
      rateLimits: createMemoryRateLimitStore(),
    };
    const limit = { windowMs: 60_000, max: 2 };
    const a = createTestApp({ store, rateLimit: limit });
    const b = createTestApp({ store, rateLimit: limit });
    const { access_token } = await a.authorize("u1");
    expect((await a.rawCall(access_token, "list_notes")).status).toBe(200);
    expect((await b.rawCall(access_token, "list_notes")).status).toBe(200);
    expect((await a.rawCall(access_token, "list_notes")).status).toBe(429);
  });
});
