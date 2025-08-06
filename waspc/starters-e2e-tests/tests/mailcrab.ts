import { type Page } from "@playwright/test";

// Full Mailcrab API response types can be found here:
// https://github.com/tweedegolf/mailcrab/blob/main/frontend/src/types.rs

interface MailCrabMessageMetadata {
  id: string;
  from: MailCrabAddress;
  to: MailCrabAddress[];
}

interface MailCrabMessage extends MailCrabMessageMetadata {
  text: string;
}

interface MailCrabAddress {
  name?: string;
  email?: string;
}

const MAILCRAB_API_BASE_URL = "http://localhost:1080";

export async function getMailCrabEmailVerificationLink(
  page: Page,
  email: string,
): Promise<string> {
  const mailCrabMessagesMetadata = await getMailCrabMessagesMetadata(page);

  const mailCrabMessageMetadata = mailCrabMessagesMetadata.find(
    (message) => message.to.at(0)?.email === email,
  );
  if (!mailCrabMessageMetadata) {
    throw new Error(`MailCrab message for email '${email}' not found`);
  }

  const mailCrabMessage = await getMailCrabMessage(
    page,
    mailCrabMessageMetadata.id,
  );

  const matchedEmails = mailCrabMessage.text.match(/https?:\/\/[^\s]+/);
  if (matchedEmails === null) {
    throw new Error(
      `No email verification link found inside of MailCrab message for email '${email}'`,
    );
  }

  return matchedEmails[0];
}

async function getMailCrabMessagesMetadata(
  page: Page,
): Promise<MailCrabMessageMetadata[]> {
  const mailCrabMessagesResponse = await page.request.get(
    `${MAILCRAB_API_BASE_URL}/api/messages`,
  );
  return (await mailCrabMessagesResponse.json()) as MailCrabMessageMetadata[];
}

async function getMailCrabMessage(
  page: Page,
  messageId: string,
): Promise<MailCrabMessage> {
  const mailCrabMessageDetailsResponse = await page.request.get(
    `${MAILCRAB_API_BASE_URL}/api/message/${messageId}`,
  );
  return (await mailCrabMessageDetailsResponse.json()) as MailCrabMessage;
}
