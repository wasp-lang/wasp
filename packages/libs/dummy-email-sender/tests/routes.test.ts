import * as fs from "node:fs";
import type { Server } from "node:http";
import type { AddressInfo } from "node:net";
import * as os from "node:os";
import * as path from "node:path";

import express from "express";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { createDevMailbox, type DevMailbox } from "../src/core/devMailbox";
import type { NewDevMailboxEmail } from "../src/core/email";
import { registerDevMailboxRoutes } from "../src/routes";

const BASE_PATH = "/__wasp/mailbox";

let directoryPath: string;
let mailbox: DevMailbox;
let server: Server;
let apiUrl: string;

beforeEach(async () => {
  directoryPath = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-dev-mailbox-"));
  mailbox = createDevMailbox({ directoryPath, maxEmails: 500 });

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

function makeEmail(
  overrides: Partial<NewDevMailboxEmail> = {},
): NewDevMailboxEmail {
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

  it("lists emails with filters and a limit", async () => {
    mailbox.add(makeEmail({ to: "a@app.test" }));
    const newest = mailbox.add(makeEmail({ to: "a@app.test" }))!;
    mailbox.add(makeEmail({ to: "b@app.test" }));

    const response = await fetch(`${apiUrl}/emails?search=a@app.test&limit=1`);

    expect(await response.json()).toEqual({
      emails: [
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
      `${apiUrl}/emails?search=code&searchIn=subject,body`,
    );
    const subjectResponse = await fetch(
      `${apiUrl}/emails?search=code&searchIn=subject`,
    );

    expect(await bodyResponse.json()).toMatchObject({
      emails: [expect.anything()],
    });
    expect(await subjectResponse.json()).toEqual({ emails: [] });
  });

  it("rejects unknown search fields", async () => {
    const response = await fetch(`${apiUrl}/emails?search=a&searchIn=cc`);

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_filter" },
    });
  });

  it("waits for a email whose body matches the search", async () => {
    mailbox.add(makeEmail({ text: "Nothing here" }));
    const response = callAction("emails.waitFor", {
      search: "code is",
      searchIn: ["body"],
      timeoutMs: 2000,
    });
    await new Promise((resolve) => setTimeout(resolve, 50));
    const email = mailbox.add(makeEmail({ text: "Your code is 42" }))!;

    expect(await (await response).json()).toEqual({ email });
  });

  it("rejects an invalid limit", async () => {
    const response = await fetch(`${apiUrl}/emails?limit=0`);

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_limit" },
    });
  });

  it("returns a email and its HTML", async () => {
    const email = mailbox.add(makeEmail())!;

    const emailResponse = await fetch(`${apiUrl}/emails/${email.id}`);
    const htmlResponse = await fetch(`${apiUrl}/emails/${email.id}/html`);

    expect(await emailResponse.json()).toEqual({ email });
    expect(htmlResponse.headers.get("content-security-policy")).toBe(
      "script-src 'none'",
    );
    expect(await htmlResponse.text()).toBe(email.html);
  });

  it("returns 404 for a missing email", async () => {
    const response = await fetch(`${apiUrl}/emails/missing`);

    expect(response.status).toBe(404);
    expect(await response.json()).toMatchObject({
      error: { code: "email_not_found" },
    });
  });

  it("waits for a email", async () => {
    const response = callAction("emails.waitFor", {
      search: "late@app.test",
      timeoutMs: 2000,
    });
    await new Promise((resolve) => setTimeout(resolve, 50));
    const email = mailbox.add(makeEmail({ to: "late@app.test" }))!;

    expect(await (await response).json()).toEqual({ email });
  });

  it("times out waiting with 408", async () => {
    const response = await callAction("emails.waitFor", { timeoutMs: 10 });

    expect(response.status).toBe(408);
    expect(await response.json()).toMatchObject({ error: { code: "timeout" } });
  });

  it("rejects a timeout over 5 minutes", async () => {
    const response = await callAction("emails.waitFor", {
      timeoutMs: 5 * 60_000 + 1,
    });

    expect(response.status).toBe(400);
    expect(await response.json()).toMatchObject({
      error: { code: "invalid_timeout" },
    });
  });

  it("deletes a email", async () => {
    const email = mailbox.add(makeEmail())!;

    const response = await callAction("emails.delete", { id: email.id });
    const missingResponse = await callAction("emails.delete", {
      id: email.id,
    });

    expect(await response.json()).toEqual({ ok: true });
    expect(missingResponse.status).toBe(404);
  });

  it("clears all emails", async () => {
    mailbox.add(makeEmail());

    await callAction("emails.clear", {});

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
    const email = mailbox.add(makeEmail())!;
    const { value } = await reader.read();
    abortController.abort();

    expect(response.headers.get("content-type")).toBe("text/event-stream");
    expect(decoder.decode(value)).toContain(`event: added\ndata: `);
    expect(decoder.decode(value)).toContain(email.id);
  });
});
