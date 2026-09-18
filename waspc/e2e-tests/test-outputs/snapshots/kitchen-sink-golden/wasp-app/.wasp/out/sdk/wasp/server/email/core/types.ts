import type { SentMessageInfo, EmailFromField } from '@wasp.sh/lib-sdk-core/node'
export { type EmailProvider, type SMTPEmailProvider, type SendGridProvider, type MailgunEmailProvider, type ResendEmailProvider, type DummyEmailProvider, type SentMessageInfo, type EmailFromField } from '@wasp.sh/lib-sdk-core/node'

// PUBLIC API
export type EmailSender = {
  send: (email: Email) => Promise<SentMessageInfo>;
};

// PUBLIC API
export type Email = {
  from?: EmailFromField;
  to: string;
  subject: string;
  text: string;
  html: string;
};
