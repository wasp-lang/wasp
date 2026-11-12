import { createDevMailboxClient } from "wasp/server/email/devMailbox";
import { WASP_SERVER_URL } from "../playwright.config";

const devMailbox = createDevMailboxClient(WASP_SERVER_URL);

export async function getDevMailboxEmailVerificationLink(
  email: string,
): Promise<string> {
  const verificationEmail = await devMailbox.waitFor(
    { search: email, searchIn: ["to"] },
    { timeoutMs: 10_000 },
  );
  if (verificationEmail === undefined) {
    throw new Error(`No dev mailbox email for '${email}' arrived in time.`);
  }
  const link = verificationEmail.links.at(0);
  if (link === undefined) {
    throw new Error(
      `No email verification link found inside of dev mailbox email for '${email}'`,
    );
  }
  return link;
}
