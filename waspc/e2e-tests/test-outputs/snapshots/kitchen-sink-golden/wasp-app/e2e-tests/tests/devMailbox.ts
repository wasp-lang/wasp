import { type Page } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

// The dev mailbox keeps emails sent with the Dummy email sender. See the
// "Dev mailbox" section of the Wasp docs on sending emails.
const DEV_MAILBOX_API_URL = `${WASP_SERVER_URL}/__wasp/mailbox/api`;

interface DevMailboxMessage {
  id: string;
  to: string;
  subject: string;
  links: string[];
}

export async function getDevMailboxEmailVerificationLink(
  page: Page,
  email: string,
): Promise<string> {
  const response = await page.request.post(
    `${DEV_MAILBOX_API_URL}/messages.waitFor`,
    { data: { search: email, searchIn: ["to"], timeoutMs: 10_000 } },
  );
  if (!response.ok()) {
    throw new Error(
      `Dev mailbox message for email '${email}' not found: ${await response.text()}`,
    );
  }

  const { message } = (await response.json()) as {
    message: DevMailboxMessage;
  };
  const link = message.links.at(0);
  if (link === undefined) {
    throw new Error(
      `No email verification link found inside of dev mailbox message for email '${email}'`,
    );
  }
  return link;
}
