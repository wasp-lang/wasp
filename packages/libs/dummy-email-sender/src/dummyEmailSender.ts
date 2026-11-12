import type { DevMailbox } from "./mailbox/devMailbox.js";
import type { EmailFromField } from "./types.js";

export type DummyEmail = {
  /** Falls back to the sender's `defaultFrom`. */
  from?: EmailFromField;
  to: string;
  subject: string;
  text: string;
  html: string;
};

export type DummyEmailSender = {
  send: (email: DummyEmail) => Promise<{ success: true }>;
};

export type DummyEmailSenderOptions = {
  mailbox: DevMailbox;
  defaultFrom: EmailFromField;
  /** URL of the inbox page, e.g. `http://localhost:3001/__wasp/mailbox`. */
  mailboxUrl: string;
  log?: (line: string) => void;
};

/**
 * Doesn't send emails. It logs them and keeps them in the dev mailbox, where
 * developers, tests, and agents can read them.
 */
export function createDummyEmailSender({
  mailbox,
  defaultFrom,
  mailboxUrl,
  log = console.log,
}: DummyEmailSenderOptions): DummyEmailSender {
  return {
    send: async (email) => {
      const from = email.from ?? defaultFrom;

      log(yellow("╔═══════════════════════╗"));
      log(yellow("║ Dummy email sender ✉️  ║"));
      log(yellow("╚═══════════════════════╝"));
      log(`From:    ${formatFromField(from)}`);
      log(`To:      ${email.to}`);
      log(`Subject: ${email.subject}`);
      log(yellow("═════════ Text ═════════"));
      log(email.text);
      log(yellow("═════════ HTML ═════════"));
      log(email.html);
      log(yellow("════════════════════════"));

      const message = mailbox.add({ ...email, from });
      if (message !== undefined) {
        log(`Inbox:   ${mailboxUrl}#${message.id}`);
        log(`API:     ${mailboxUrl}/api/messages/${message.id}`);
        log(`File:    ${mailbox.directoryPath}/${message.id}.json`);
        log(yellow("════════════════════════"));
      }

      return { success: true };
    },
  };
}

function formatFromField({ name, email }: EmailFromField): string {
  return name ? `${name} <${email}>` : email;
}

function yellow(text: string): string {
  // Respect https://no-color.org, like the rest of Wasp's console output.
  if ("NO_COLOR" in process.env) {
    return text;
  }
  return `\x1b[33m${text}\x1b[0m`;
}
