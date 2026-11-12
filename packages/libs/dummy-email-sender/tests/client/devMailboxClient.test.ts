import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { ApiError } from "../../src/api/apiError";
import {
  createDevMailboxApiClient,
  type DevMailboxClient,
} from "../../src/client/devMailboxClient";
import type { DevMailbox } from "../../src/core/devMailbox";
import {
  makeEmail,
  startMailboxServer,
  type MailboxServer,
} from "../mailboxServer";

let mailboxServer: MailboxServer;
let mailbox: DevMailbox;
let client: DevMailboxClient;

beforeEach(async () => {
  mailboxServer = await startMailboxServer();
  mailbox = mailboxServer.mailbox;
  client = createDevMailboxApiClient(mailboxServer.apiUrl);
});

afterEach(() => mailboxServer.close());

describe("createDevMailboxApiClient", () => {
  it("lists emails with a filter and a limit", async () => {
    mailbox.add(makeEmail({ to: "a@app.test" }));
    const newest = mailbox.add(makeEmail({ to: "a@app.test" }))!;
    mailbox.add(makeEmail({ to: "b@app.test", subject: "Other" }));

    const emails = await client.list(
      { search: "a@app.test", searchIn: ["to"] },
      1,
    );

    expect(emails.map((email) => email.id)).toEqual([newest.id]);
  });

  it("gets an email, or undefined when it doesn't exist", async () => {
    const email = mailbox.add(makeEmail())!;

    await expect(client.get(email.id)).resolves.toEqual(email);
    await expect(client.get("missing")).resolves.toBeUndefined();
  });

  it("waits for an email", async () => {
    const waiting = client.waitFor(
      { search: "late@app.test", searchIn: ["to"] },
      { timeoutMs: 2000 },
    );
    await new Promise((resolve) => setTimeout(resolve, 50));
    const email = mailbox.add(makeEmail({ to: "late@app.test" }))!;

    await expect(waiting).resolves.toEqual(email);
  });

  it("resolves with undefined when waiting times out", async () => {
    await expect(
      client.waitFor({}, { timeoutMs: 10 }),
    ).resolves.toBeUndefined();
  });

  it("deletes an email and reports whether it existed", async () => {
    const email = mailbox.add(makeEmail())!;

    await expect(client.delete(email.id)).resolves.toBe(true);
    await expect(client.delete(email.id)).resolves.toBe(false);
  });

  it("clears all emails", async () => {
    mailbox.add(makeEmail());

    await client.clear();

    expect(mailbox.list()).toEqual([]);
  });

  it("rejects invalid requests with an ApiError", async () => {
    await expect(client.list({}, 0)).rejects.toMatchObject({
      constructor: ApiError,
      code: "invalid_limit",
    });
  });
});
