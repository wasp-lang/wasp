import { initEmailSender, SMTPEmailProvider } from "./core/index.js";
import type { SendGridProvider } from "./core/types.js";

const smtpEmailProvider: SMTPEmailProvider = {
    type: "smtp",
    host: process.env.SMTP_HOST,
    port: parseInt(process.env.SMTP_PORT, 10),
    username: process.env.SMTP_USERNAME,
    password: process.env.SMTP_PASSWORD,
}

const sendGridProvider: SendGridProvider = {
    type: "sendgrid",
    apiKey: process.env.SENDGRID_API_KEY,
  };

export const emailSender = initEmailSender(smtpEmailProvider);
