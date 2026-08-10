import { MailtrapClient } from "mailtrap";
import { getDefaultFromField } from "../helpers.js";
import type { MailtrapEmailProvider, EmailSender } from "../types";

// PRIVATE API
export function initMailtrapEmailSender(
  config: MailtrapEmailProvider
): EmailSender {
  const defaultFromField = getDefaultFromField();

  const client = new MailtrapClient({
    token: config.apiToken,
    sandbox: config.sandbox,
    testInboxId: config.testInboxId,
  });

  return {
    async send(email) {
      const fromField = email.from || defaultFromField;
      return client.send({
        from: fromField,
        to: [{ email: email.to }],
        subject: email.subject,
        text: email.text,
        html: email.html,
      });
    },
  };
}