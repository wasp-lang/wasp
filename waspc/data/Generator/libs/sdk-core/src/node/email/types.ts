export type EmailProvider =
  | SMTPEmailProvider
  | SendGridProvider
  | MailgunEmailProvider
  | ResendEmailProvider
  | DummyEmailProvider;

export type SMTPEmailProvider = {
  type: "smtp";
  host: string;
  port: number;
  username: string;
  password: string;
};

export type SendGridProvider = {
  type: "sendgrid";
  apiKey: string;
};

export type MailgunEmailProvider = {
  type: "mailgun";
  apiKey: string;
  domain: string;
  apiUrl?: string;
};

export type ResendEmailProvider = {
  type: "resend";
  apiKey: string;
};

export type DummyEmailProvider = {
  type: "dummy";
};

export type SentMessageInfo = any;

export type EmailFromField = {
  name?: string;
  email: string;
};
