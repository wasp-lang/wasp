import {
  DEV_MAILBOX_API_BASE_PATH,
  devMailboxApiPaths,
} from "./api/apiPaths.js";
import type { DevMailbox } from "./core/devMailbox.js";
import type { NewDevMailboxEmail } from "./core/email.js";
import type { EmailFromField } from "./emailFromField.js";

export type DummyEmail = Omit<NewDevMailboxEmail, "from"> & {
  /** Falls back to the sender's `defaultFrom`. */
  from?: EmailFromField;
};

type DummyEmailSender = {
  send: (email: DummyEmail) => Promise<{ success: true }>;
};

export type DummyEmailSenderOptions = {
  mailbox: DevMailbox;
  defaultFrom: EmailFromField;
  /** e.g. `http://localhost:3001/__wasp/mailbox` */
  mailboxUrl: string;
  log?: (line: string) => void;
};

/** Logs emails and keeps them in the mailbox instead of sending them. */
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

      const storedEmail = mailbox.add({ ...email, from });
      if (storedEmail !== undefined) {
        log(`Inbox:   ${mailboxUrl}#${storedEmail.id}`);
        log(
          `API:     ${mailboxUrl}${DEV_MAILBOX_API_BASE_PATH}${devMailboxApiPaths.email(storedEmail.id)}`,
        );
        log(`File:    ${mailbox.directoryPath}/${storedEmail.id}.json`);
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
  // https://no-color.org
  if ("NO_COLOR" in process.env) {
    return text;
  }
  return `\x1b[33m${text}\x1b[0m`;
}
