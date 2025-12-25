import Mailgun from 'mailgun';
import { getDefaultFromField } from "../helpers";
import type { EmailSender, MailgunEmailProvider } from "../types";

// PRIVATE API
export function initMailgunEmailSender(
  config: MailgunEmailProvider
): EmailSender {
  const defaultFromField = getDefaultFromField();

  const mailgun = new Mailgun(FormData);

  const mailer = mailgun.client({
    username: 'api',
    key: config.apiKey,
    url: config.apiUrl,
  });

  return {
    async send(email) {
      const fromField = email.from || defaultFromField;
      return mailer.messages.create(config.domain, {
        from: `${fromField.name} <${fromField.email}>`,
        to: [email.to],
        subject: email.subject,
        text: email.text,
        html: email.html,
      })
    },
  };
}
