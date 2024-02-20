import { DummyEmailProvider, EmailSender } from "../types";
import { getDefaultFromField } from "../helpers.js";

const yellowColor = "\x1b[33m%s\x1b[0m";

// PRIVATE API
export function initDummyEmailSender(
    config?: DummyEmailProvider,
): EmailSender {
    const defaultFromField = getDefaultFromField();
    return {
        send: async (email) => {
            const fromField = email.from || defaultFromField;

            console.log(yellowColor, '╔═══════════════════════╗');
            console.log(yellowColor, '║ Dummy email sender ✉️  ║');
            console.log(yellowColor, '╚═══════════════════════╝');
            console.log(`From:    ${fromField.name} <${fromField.email}>`);
            console.log(`To:      ${email.to}`);
            console.log(`Subject: ${email.subject}`);
            console.log(yellowColor, '═════════ Text ═════════');
            console.log(email.text);
            console.log(yellowColor, '═════════ HTML ═════════');
            console.log(email.html);
            console.log(yellowColor, '════════════════════════');

            return {
                success: true,
            };
        }
    }
}
