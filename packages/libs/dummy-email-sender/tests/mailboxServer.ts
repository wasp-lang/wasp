import * as fs from "node:fs";
import type { Server } from "node:http";
import type { AddressInfo } from "node:net";
import * as os from "node:os";
import * as path from "node:path";

import express from "express";

import { createDevMailbox, type DevMailbox } from "../src/core/devMailbox";
import type { NewDevMailboxEmail } from "../src/core/email";
import { registerDevMailboxRoutes } from "../src/routes";

export const MAILBOX_PATH = "/__wasp/mailbox";

export type MailboxServer = {
  mailbox: DevMailbox;
  mailboxUrl: string;
  apiUrl: string;
  close: () => Promise<void>;
};

/** A mailbox in a temporary directory, served like the generated server does. */
export async function startMailboxServer({
  shouldParseJson = true,
} = {}): Promise<MailboxServer> {
  const directoryPath = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-dev-mailbox-"),
  );
  const mailbox = createDevMailbox({ directoryPath, maxEmails: 500 });

  const router = express.Router();
  if (shouldParseJson) {
    router.use(express.json());
  }
  registerDevMailboxRoutes(router, mailbox, { appName: "TestApp" });
  const app = express().use(MAILBOX_PATH, router);

  const server = await new Promise<Server>((resolve) => {
    const listeningServer = app.listen(0, () => resolve(listeningServer));
  });
  const { port } = server.address() as AddressInfo;
  const mailboxUrl = `http://localhost:${port}${MAILBOX_PATH}`;

  return {
    mailbox,
    mailboxUrl,
    apiUrl: `${mailboxUrl}/api`,
    close: async () => {
      server.closeAllConnections();
      await new Promise((resolve) => server.close(resolve));
      fs.rmSync(directoryPath, { recursive: true, force: true });
    },
  };
}

export function makeEmail(
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
