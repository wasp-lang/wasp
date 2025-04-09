import SendGrid from "@sendgrid/mail";
import { getDefaultFromField } from "../helpers.js";
import type { SendGridProvider, EmailSender } from "../types";

// PRIVATE API
export function initSendGridEmailSender(
  provider: SendGridProvider
): EmailSender {
  SendGrid.setApiKey(provider.apiKey);

  const defaultFromField = getDefaultFromField();

  return {
    async send(email) {
      const fromField = email.from || defaultFromField;
      return SendGrid.send({
        from: {
          email: fromField.email,
          name: fromField.name,
        },
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html,
      }).catch((error) => {
        // SendGrid can return an array of errors in its response,
        // but the client doesn't surface them appropriately.
        // If we find it, we will throw an AggregateError with all the errors
        // so that they are all printed. Otherwise, we will throw the
        // original error.

        const responseErrors = error?.response?.body?.errors;
        if (responseErrors && Array.isArray(responseErrors)) {
          throw new AggregateError(
            [...responseErrors, error],
            `SendGrid error: ${error.message}`
          );
        } else {
          throw error;
        }
      });
    },
  };
}
