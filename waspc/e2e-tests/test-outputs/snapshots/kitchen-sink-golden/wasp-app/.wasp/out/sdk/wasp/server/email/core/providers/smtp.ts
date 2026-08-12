import { createTransport } from "nodemailer";
import { formatFromField, getDefaultFromField } from "../helpers.js";
import type { SMTPEmailProvider, EmailSender } from "../types";

// PRIVATE API
export function initSmtpEmailSender(config: SMTPEmailProvider): EmailSender {
  // TODO: Make the transport a stateful resource (`wasp/server/lifecycle`), so
  // that it is closed when the server stops and reused across reloads in
  // development. Nodemailer's default SMTP transport opens a connection per
  // message and closes it again, so nothing leaks meanwhile.
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
