import * as fs from "node:fs";
import type { Server } from "node:http";
import type { AddressInfo } from "node:net";
import * as os from "node:os";
import * as path from "node:path";

import express from "express";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { createDevMailbox, type DevMailbox } from "../src/core/devMailbox";
import type { DevMailboxEmail } from "../src/core/message";
import { registerDevMailboxRoutes } from "../src/routes";

const BASE_PATH = "/__wasp/mailbox";

let directoryPath: string;
let mailbox: DevMailbox;
let server: Server;
let apiUrl: string;

beforeEach(async () => {
  directoryPath = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-dev-mailbox-"));
  mailbox = createDevMailbox({ directoryPath, maxMessages: 500 });

  const router = express.Router();
  router.use(express.json());
  registerDevMailboxRoutes(router, mailbox, { appName: "TestApp" });
  const app = express().use(BASE_PATH, router);

  server = await new Promise<Server>((resolve) => {
    const listeningServer = app.listen(0, () => resolve(listeningServer));
  });
  const { port } = server.address() as AddressInfo;
  apiUrl = `http://localhost:${port}${BASE_PATH}/api`;
});

afterEach(async () => {
  server.closeAllConnections();
  await new Promise((resolve) => server.close(resolve));
  fs.rmSync(directoryPath, { recursive: true, force: true });
});

function makeEmail(overrides: Partial<DevMailboxEmail> = {}): DevMailboxEmail {
  return {
    from: { email: "app@app.test" },
    to: "user@app.test",
    subject: "Verify your email",
    text: "Verify at https://app.test/verify",
    html: `<a href="https://app.test/verify">Verify</a>`,
    ...overrides,
  };
}

function callAction(action: string, body: unknown) {
  return fetch(`${apiUrl}/${action}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
}

describe("dev mailbox routes", () => {
  it("serves the inbox page pointing at the API", async () => {
    const response = await fetch(apiUrl.replace(/\/api$/, ""));

    expect(response.headers.get("content-type")).toMatch(/text\/html/);
    const page = await response.text();
    expect(page).toContain(`"${BASE_PATH}/api"`);
    expect(page).toContain("TestApp");
  });

  it("lists messages with filters and a limit", async () => {
    mailbox.add(makeEmail({ to: "a@app.test" }));
    const newest = mailbox.add(makeEmail({ to: "a@app.test" }))!;
    mailbox.add(makeEmail({ to: "b@app.test" }));

    const response = await fetch(
      `${apiUrl}/messages?search=a@app.test&limit=1`,
    );

    expect(await response.json()).toEqual({
      messages: [
        {
          id: newest.id,
          sentAt: newest.sentAt,
          from: newest.from,
          to: "a@app.test",
          subject: newest.subject,
        },
      ],
    });
  });

  it("searches in the fields from a comma-separated list", async () => {
    mailbox.add(makeEmail({ subject: "Hello", text: "Your code is 42" }));

    const bodyResponse = await fetch(
      `${apiUrl}/messages?search=code&searchIn=subject,body`,
    );
    const subjectResponse = await fetch(
      `${apiUrl}/messages?search=code&searchIn=subject`,
    );

    expect(await bodyResponse.json()).toMatchObject({
      messages: [expect.anything()],
    });
    expect(await subjectResponse.json()).toEqual({ messages: [] });
  });

  it("rejects unknown search fields", async () => {
    const response = await fetch(`${apiUrl}/messages?search=a&searchIn=cc`);

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_filter" },
    });
  });

  it("waits for a message whose body matches the search", async () => {
    mailbox.add(makeEmail({ text: "Nothing here" }));
    const response = callAction("messages.waitFor", {
      search: "code is",
      searchIn: ["body"],
      timeoutMs: 2000,
    });
    await new Promise((resolve) => setTimeout(resolve, 50));
    const message = mailbox.add(makeEmail({ text: "Your code is 42" }))!;

    expect(await (await response).json()).toEqual({ message });
  });

  it("rejects an invalid limit", async () => {
    const response = await fetch(`${apiUrl}/messages?limit=0`);

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_limit" },
    });
  });

  it("returns a message and its HTML", async () => {
    const message = mailbox.add(makeEmail())!;

    const messageResponse = await fetch(`${apiUrl}/messages/${message.id}`);
    const htmlResponse = await fetch(`${apiUrl}/messages/${message.id}/html`);

    expect(await messageResponse.json()).toEqual({ message });
    expect(htmlResponse.headers.get("content-security-policy")).toBe(
      "script-src 'none'",
    );
    expect(await htmlResponse.text()).toBe(message.html);
  });

  it("returns 404 for a missing message", async () => {
    const response = await fetch(`${apiUrl}/messages/missing`);

    expect(response.status).toBe(404);
    expect(await response.json()).toMatchObject({
      error: { code: "message_not_found" },
    });
  });

  it("waits for a message", async () => {
    const response = callAction("messages.waitFor", {
      search: "late@app.test",
      timeoutMs: 2000,
    });
    await new Promise((resolve) => setTimeout(resolve, 50));
    const message = mailbox.add(makeEmail({ to: "late@app.test" }))!;

    expect(await (await response).json()).toEqual({ message });
  });

  it("times out waiting with 408", async () => {
    const response = await callAction("messages.waitFor", { timeoutMs: 10 });

    expect(response.status).toBe(408);
    expect(await response.json()).toMatchObject({ error: { code: "timeout" } });
  });

  it("rejects a timeout over 5 minutes", async () => {
    const response = await callAction("messages.waitFor", {
      timeoutMs: 5 * 60_000 + 1,
    });

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_timeout" },
    });
  });

  it("deletes a message", async () => {
    const message = mailbox.add(makeEmail())!;

    const response = await callAction("messages.delete", { id: message.id });
    const missingResponse = await callAction("messages.delete", {
      id: message.id,
    });

    expect(await response.json()).toEqual({ ok: true });
    expect(missingResponse.status).toBe(404);
  });

  it("clears all messages", async () => {
    mailbox.add(makeEmail());

    await callAction("messages.clear", {});

    expect(mailbox.list()).toEqual([]);
  });

  it("streams events", async () => {
    const abortController = new AbortController();
    const response = await fetch(`${apiUrl}/events`, {
      signal: abortController.signal,
    });
    const reader = response.body!.getReader();
    const decoder = new TextDecoder();

    await reader.read();
    const message = mailbox.add(makeEmail())!;
    const { value } = await reader.read();
    abortController.abort();

    expect(response.headers.get("content-type")).toBe("text/event-stream");
    expect(decoder.decode(value)).toContain(`event: added\ndata: `);
    expect(decoder.decode(value)).toContain(message.id);
  });
});
