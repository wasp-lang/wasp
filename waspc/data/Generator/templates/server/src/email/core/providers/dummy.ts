import { EmailSender } from "../types.js";
import { getDefaultFromField } from "../helpers.js";

export function initDummyEmailSender(): EmailSender {
    const defaultFromField = getDefaultFromField();
    return {
        send: async (email) => {
            const fromField = email.from || defaultFromField;
            console.log('Test email (not sent):', {
                from: {
                    email: fromField.email,
                    name: fromField.title,
                },
                to: email.to,
                subject: email.subject,
                text: email.text,
                html: email.html,
            });
            return {
                success: true,
            };
        }
    }
}
