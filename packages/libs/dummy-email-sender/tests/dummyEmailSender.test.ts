import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createDevMailbox } from "../src/core/devMailbox";
import { createDummyEmailSender } from "../src/dummyEmailSender";

const MAILBOX_URL = "http://localhost:3001/__wasp/mailbox";
const EMAIL = {
  to: "user@app.test",
  subject: "Verify your email",
  text: "Verify at https://app.test/verify",
  html: `<a href="https://app.test/verify">Verify</a>`,
};

let directoryPath: string;
let lines: string[];

beforeEach(() => {
  directoryPath = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-dev-mailbox-"));
  lines = [];
});

afterEach(() => {
  vi.unstubAllEnvs();
  fs.rmSync(directoryPath, { recursive: true, force: true });
});

function createSender(maxMessages = 500) {
  const mailbox = createDevMailbox({ directoryPath, maxMessages });
  const sender = createDummyEmailSender({
    mailbox,
    defaultFrom: { name: "App", email: "app@app.test" },
    mailboxUrl: MAILBOX_URL,
    log: (line) => lines.push(line),
  });
  return { mailbox, sender };
}

describe("createDummyEmailSender", () => {
  it("keeps the email in the mailbox and logs where to find it", async () => {
    const { mailbox, sender } = createSender();

    await expect(sender.send(EMAIL)).resolves.toEqual({ success: true });

    const [summary] = mailbox.list();
    expect(summary).toMatchObject({ to: "user@app.test" });
    expect(lines).toContain("To:      user@app.test");
    expect(lines).toContain(`Inbox:   ${MAILBOX_URL}#${summary!.id}`);
    expect(lines).toContain(
      `API:     ${MAILBOX_URL}/api/messages/${summary!.id}`,
    );
    expect(lines).toContain(`File:    ${directoryPath}/${summary!.id}.json`);
  });

  it("uses the default sender when the email has none", async () => {
    const { mailbox, sender } = createSender();

    await sender.send(EMAIL);

    expect(mailbox.list()[0]?.from).toEqual({
      name: "App",
      email: "app@app.test",
    });
    expect(lines).toContain("From:    App <app@app.test>");
  });

  it("uses the email's sender when it has one", async () => {
    const { mailbox, sender } = createSender();

    await sender.send({ ...EMAIL, from: { email: "other@app.test" } });

    expect(mailbox.list()[0]?.from).toEqual({ email: "other@app.test" });
    expect(lines).toContain("From:    other@app.test");
  });

  it("only logs the email when the mailbox keeps none", async () => {
    const { mailbox, sender } = createSender(0);

    await sender.send(EMAIL);

    expect(mailbox.list()).toEqual([]);
    expect(lines).toContain("To:      user@app.test");
    expect(lines.some((line) => line.startsWith("Inbox:"))).toBe(false);
  });

  it("colors the banner", async () => {
    vi.stubEnv("NO_COLOR", undefined);
    const { sender } = createSender();

    await sender.send(EMAIL);

    expect(lines[0]).toMatch(/^\x1b\[33m/);
  });

  it("doesn't color anything when NO_COLOR is set", async () => {
    vi.stubEnv("NO_COLOR", "1");
    const { sender } = createSender();

    await sender.send(EMAIL);

    expect(lines.some((line) => line.includes("\x1b["))).toBe(false);
  });
});
