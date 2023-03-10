import { createTransport } from "nodemailer";
import { createFromEmailString } from "../helpers.js";
import type { SMTPEmailProvider, EmailSender } from "../types.js";

export function initSmtpEmailSender(config: SMTPEmailProvider): EmailSender {
  const transporter = createTransport({
    host: config.host,
    port: config.port,
    auth: {
      user: config.username,
      pass: config.password,
    },
  });

  return {
    async send(email) {
      return transporter.sendMail({
        from: createFromEmailString(email.from),
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html,
      });
    },
  };
}
