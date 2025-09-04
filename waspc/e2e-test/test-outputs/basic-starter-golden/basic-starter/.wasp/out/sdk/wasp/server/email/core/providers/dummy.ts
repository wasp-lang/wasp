import { DummyEmailProvider, EmailSender } from "../types";
import { getDefaultFromField } from "../helpers.js";

import { getColorizedConsoleFormatString } from 'wasp/universal/ansiColors'

const yellowColorFormatString = getColorizedConsoleFormatString('yellow');

// PRIVATE API
export function initDummyEmailSender(
    config?: DummyEmailProvider,
): EmailSender {
    const defaultFromField = getDefaultFromField();
    return {
        send: async (email) => {
            const fromField = email.from || defaultFromField;

            console.log(yellowColorFormatString, '╔═══════════════════════╗');
            console.log(yellowColorFormatString, '║ Dummy email sender ✉️  ║');
            console.log(yellowColorFormatString, '╚═══════════════════════╝');
            console.log(`From:    ${fromField.name} <${fromField.email}>`);
            console.log(`To:      ${email.to}`);
            console.log(`Subject: ${email.subject}`);
            console.log(yellowColorFormatString, '═════════ Text ═════════');
            console.log(email.text);
            console.log(yellowColorFormatString, '═════════ HTML ═════════');
            console.log(email.html);
            console.log(yellowColorFormatString, '════════════════════════');

            return {
                success: true,
            };
        }
    }
}
