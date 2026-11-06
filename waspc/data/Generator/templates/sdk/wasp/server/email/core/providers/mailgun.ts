import Mailgun from 'mailgun.js';
import { formatFromField } from '@wasp/lib-sdk-core/node'
import { getDefaultFromField } from "../helpers.js";
import type { MailgunEmailProvider, EmailSender } from "../types";

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
      return mailer.messages.create(config.domain, {
        from: formatFromField(email.from || defaultFromField),
        to: [email.to],
        subject: email.subject,
        text: email.text,
        html: email.html,
      })
    },
  };
}
