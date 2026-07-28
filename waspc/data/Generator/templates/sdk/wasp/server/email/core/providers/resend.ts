import { Resend } from 'resend';
import { formatFromField, getDefaultFromField } from "../helpers.js";
import type { ResendEmailProvider, EmailSender } from "../types";

// PRIVATE API
export function initResendEmailSender(
  config: ResendEmailProvider
): EmailSender {
  const defaultFromField = getDefaultFromField();

  const client = new Resend(config.apiKey);

  return {
    async send(email) {
      // Resend's SDK resolves with `{ data, error }` instead of throwing on
      // API errors, so we surface the error ourselves to match the other
      // providers (and so callers' error handling actually fires).
      const { data, error } = await client.emails.send({
        from: formatFromField(email.from || defaultFromField),
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html,
      });
      if (error) {
        throw error;
      }
      return data;
    },
  };
}
