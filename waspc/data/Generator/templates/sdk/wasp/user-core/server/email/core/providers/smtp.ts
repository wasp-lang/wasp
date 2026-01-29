import { createTransport } from "nodemailer";
import { formatFromField, getDefaultFromField } from "../helpers";
import type { EmailSender, SMTPEmailProvider } from "../types";

// PRIVATE API
export function initSmtpEmailSender(config: SMTPEmailProvider): EmailSender {
  const transporter = createTransport({
    host: config.host,
    port: config.port,
    auth: {
      user: config.username,
      pass: config.password,
    },
  });

  const defaultFromField = getDefaultFromField();

  return {
    async send(email) {
      return transporter.sendMail({
        from: formatFromField(email.from || defaultFromField),
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html,
      });
    },
  };
}
