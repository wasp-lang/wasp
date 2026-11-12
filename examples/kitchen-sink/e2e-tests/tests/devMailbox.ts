import { type Page } from "@playwright/test";
import {
  devMailboxApiPath,
  type DevMailboxEmail,
} from "wasp/server/email/devMailbox";
import { WASP_SERVER_URL } from "../playwright.config";

export async function getDevMailboxEmailVerificationLink(
  page: Page,
  email: string,
): Promise<string> {
  const response = await page.request.post(
    `${WASP_SERVER_URL}${devMailboxApiPath}/emails.waitFor`,
    { data: { search: email, searchIn: ["to"], timeoutMs: 10_000 } },
  );
  if (!response.ok()) {
    throw new Error(
      `Dev mailbox message for email '${email}' not found: ${await response.text()}`,
    );
  }

  const { email: verificationEmail } = (await response.json()) as {
    email: DevMailboxEmail;
  };
  const link = verificationEmail.links.at(0);
  if (link === undefined) {
    throw new Error(
      `No email verification link found inside of dev mailbox message for email '${email}'`,
    );
  }
  return link;
}
