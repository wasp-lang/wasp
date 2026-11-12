import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { DevMailbox, type DevMailboxEvent } from "../src/mailbox/devMailbox";
import { MailboxDirectory } from "../src/mailbox/mailboxDirectory";
import type { DevMailboxEmail } from "../src/mailbox/message";

let directoryPath: string;

beforeEach(() => {
  directoryPath = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-dev-mailbox-"));
});

afterEach(() => {
  vi.restoreAllMocks();
  fs.rmSync(directoryPath, { recursive: true, force: true });
});

function createMailbox(maxMessages = 500): DevMailbox {
  return new DevMailbox({ directoryPath, maxMessages });
}

function makeEmail(overrides: Partial<DevMailboxEmail> = {}): DevMailboxEmail {
  return {
    from: { name: "App", email: "app@app.test" },
    to: "user@app.test",
    subject: "Verify your email",
    text: "Verify at https://app.test/verify",
    html: `<a href="https://app.test/verify">Verify</a>`,
    ...overrides,
  };
}

describe("DevMailbox", () => {
  it("stores messages and lists them newest first", () => {
    const mailbox = createMailbox();
    const first = mailbox.add(makeEmail({ subject: "First" }))!;
    const second = mailbox.add(makeEmail({ subject: "Second" }))!;

    expect(mailbox.list().map((message) => message.id)).toEqual([
      second.id,
      first.id,
    ]);
    expect(mailbox.get(first.id)).toMatchObject({
      subject: "First",
      links: ["https://app.test/verify"],
    });
  });

  it("writes one JSON file per message", () => {
    const message = createMailbox().add(makeEmail())!;

    const content = fs.readFileSync(
      path.join(directoryPath, `${message.id}.json`),
      "utf8",
    );
    expect(JSON.parse(content)).toEqual(message);
  });

  it("keeps messages across instances", () => {
    const message = createMailbox().add(makeEmail())!;

    expect(createMailbox().get(message.id)).toEqual(message);
  });

  it("filters to messages sent after a previous one", () => {
    const mailbox = createMailbox();
    const first = mailbox.add(makeEmail())!;
    const second = mailbox.add(makeEmail())!;

    expect(mailbox.list({ after: first.id }).map((m) => m.id)).toEqual([
      second.id,
    ]);
  });

  it("searches in the chosen fields", () => {
    const mailbox = createMailbox();
    mailbox.add(
      makeEmail({
        from: { name: "Billing", email: "billing@app.test" },
        to: "maria@app.test",
        subject: "Your invoice",
        text: "Your code is 123456",
        html: `<p class="code">Your code is 123456</p>`,
      }),
    );
    const subjects = (filter: Parameters<DevMailbox["list"]>[0]) =>
      mailbox.list(filter).map((message) => message.subject);

    expect(subjects({ search: "billing" })).toEqual(["Your invoice"]);
    expect(subjects({ search: "MARIA" })).toEqual(["Your invoice"]);
    expect(subjects({ search: "123456", searchIn: ["body"] })).toEqual([
      "Your invoice",
    ]);
    expect(subjects({ search: "123456", searchIn: ["subject"] })).toEqual([]);
    // HTML tags and attributes aren't searched.
    expect(subjects({ search: "class", searchIn: ["body"] })).toEqual([]);
  });

  it("limits the listed messages", () => {
    const mailbox = createMailbox();
    mailbox.add(makeEmail());
    const newest = mailbox.add(makeEmail())!;

    expect(mailbox.list({}, 1).map((message) => message.id)).toEqual([
      newest.id,
    ]);
  });

  it("deletes the oldest messages over maxMessages", () => {
    const mailbox = createMailbox(2);
    mailbox.add(makeEmail({ subject: "1" }));
    mailbox.add(makeEmail({ subject: "2" }));
    mailbox.add(makeEmail({ subject: "3" }));

    expect(mailbox.list().map((message) => message.subject)).toEqual([
      "3",
      "2",
    ]);
    expect(fs.readdirSync(directoryPath)).toHaveLength(2);
  });

  it("keeps nothing when maxMessages is 0", () => {
    const mailbox = createMailbox(0);

    expect(mailbox.add(makeEmail())).toBeUndefined();
    expect(mailbox.list()).toEqual([]);
  });

  it("keeps everything when maxMessages is Infinity", () => {
    const mailbox = createMailbox(Infinity);
    for (let i = 0; i < 5; i++) {
      mailbox.add(makeEmail());
    }

    expect(mailbox.list()).toHaveLength(5);
  });

  it.each([-1, 1.5, NaN])("rejects maxMessages %s", (maxMessages) => {
    expect(() => createMailbox(maxMessages)).toThrow(/maxMessages/);
  });

  it("deletes a message", () => {
    const mailbox = createMailbox();
    const message = mailbox.add(makeEmail())!;

    expect(mailbox.delete(message.id)).toBe(true);
    expect(mailbox.get(message.id)).toBeUndefined();
    expect(mailbox.delete(message.id)).toBe(false);
  });

  it("refuses ids that aren't message ids", () => {
    expect(createMailbox().delete("../../etc/passwd")).toBe(false);
  });

  it("clears all messages", () => {
    const mailbox = createMailbox();
    mailbox.add(makeEmail());
    mailbox.add(makeEmail());

    mailbox.clear();

    expect(mailbox.list()).toEqual([]);
    expect(fs.readdirSync(directoryPath)).toEqual([]);
  });

  it("sees messages other processes write", () => {
    const reader = createMailbox();
    const message = createMailbox().add(makeEmail())!;

    expect(reader.get(message.id)).toEqual(message);
  });

  it("applies a lowered maxMessages to messages already on disk", () => {
    const writer = createMailbox();
    writer.add(makeEmail({ subject: "1" }));
    writer.add(makeEmail({ subject: "2" }));
    writer.add(makeEmail({ subject: "3" }));

    expect(
      createMailbox(1)
        .list()
        .map((message) => message.subject),
    ).toEqual(["3"]);
    expect(fs.readdirSync(directoryPath)).toHaveLength(1);

    expect(createMailbox(0).list()).toEqual([]);
    expect(fs.readdirSync(directoryPath)).toEqual([]);
  });

  it("never calls a subscriber before subscribe returns", () => {
    createMailbox().add(makeEmail());
    const reader = createMailbox();
    const events: DevMailboxEvent[] = [];

    const unsubscribe = reader.subscribe((event) => events.push(event));
    unsubscribe();

    expect(events).toEqual([]);
  });

  it("notifies subscribers about changes", () => {
    const mailbox = createMailbox();
    const events: DevMailboxEvent[] = [];
    const unsubscribe = mailbox.subscribe((event) => events.push(event));

    const message = mailbox.add(makeEmail())!;
    mailbox.delete(message.id);
    mailbox.clear();
    unsubscribe();
    mailbox.add(makeEmail());

    expect(events.map((event) => event.type)).toEqual([
      "added",
      "deleted",
      "cleared",
    ]);
  });

  describe("waitFor", () => {
    it("resolves with an existing matching message", async () => {
      const mailbox = createMailbox();
      const message = mailbox.add(makeEmail())!;

      await expect(
        mailbox.waitFor({ search: "user@app.test" }, { timeoutMs: 1000 }),
      ).resolves.toEqual(message);
    });

    it("waits for a matching message", async () => {
      const mailbox = createMailbox();
      const waiting = mailbox.waitFor(
        { search: "late@app.test" },
        { timeoutMs: 1000 },
      );
      mailbox.add(makeEmail({ to: "other@app.test" }));
      const message = mailbox.add(makeEmail({ to: "late@app.test" }))!;

      await expect(waiting).resolves.toEqual(message);
    });

    it("waits for a message another process writes", async () => {
      const mailbox = createMailbox();
      const waiting = mailbox.waitFor({}, { timeoutMs: 2000 });
      const message = createMailbox().add(makeEmail())!;

      await expect(waiting).resolves.toEqual(message);
    });

    it("resolves when a message appears while the wait is set up", async () => {
      const message = createMailbox().add(makeEmail())!;
      // The first read misses the message, as if another process wrote it
      // right after the lookup.
      const listMessageIds = MailboxDirectory.prototype.listMessageIds;
      vi.spyOn(MailboxDirectory.prototype, "listMessageIds")
        .mockImplementationOnce(() => [])
        .mockImplementation(function (this: MailboxDirectory) {
          return listMessageIds.call(this);
        });

      await expect(
        createMailbox().waitFor({}, { timeoutMs: 2000 }),
      ).resolves.toEqual(message);
    });

    it("resolves with the newest of several messages found at once", async () => {
      const reader = createMailbox();
      const waiting = reader.waitFor({}, { timeoutMs: 2000 });
      const writer = createMailbox();
      writer.add(makeEmail({ subject: "Older" }));
      const newest = writer.add(makeEmail({ subject: "Newer" }))!;

      await expect(waiting).resolves.toEqual(newest);
    });

    it("resolves with undefined when the signal is already aborted", async () => {
      const abortController = new AbortController();
      abortController.abort();

      await expect(
        createMailbox().waitFor(
          {},
          { timeoutMs: 1000, signal: abortController.signal },
        ),
      ).resolves.toBeUndefined();
    });

    it("resolves with undefined on timeout", async () => {
      await expect(
        createMailbox().waitFor({}, { timeoutMs: 10 }),
      ).resolves.toBeUndefined();
    });

    it("resolves with undefined on abort", async () => {
      const abortController = new AbortController();
      const waiting = createMailbox().waitFor(
        {},
        { timeoutMs: 1000, signal: abortController.signal },
      );
      abortController.abort();

      await expect(waiting).resolves.toBeUndefined();
    });
  });
});
