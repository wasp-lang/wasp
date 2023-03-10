import { SentMessageInfo } from "nodemailer";

export type EmailProvider = SMTPEmailProvider | SendGridProvider;

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

export type EmailProviderConstructor = (provider: EmailProvider) => EmailSender;

export type EmailSender = {
  send: (email: Email) => Promise<SentMessageInfo>;
};

export type Email = {
  from: {
    title?: string;
    email: string;
  };
  to: string;
  subject: string;
  text: string;
  html: string;
};
