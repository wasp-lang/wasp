import { DummyEmailProvider, EmailSender } from "../types";
import { getDefaultFromField } from "../helpers.js";

import { getConsoleLogColorTemplate } from 'wasp/universal/ansiColors'

const yellowColorTemplate = getConsoleLogColorTemplate('yellow');

// PRIVATE API
export function initDummyEmailSender(
    config?: DummyEmailProvider,
): EmailSender {
    const defaultFromField = getDefaultFromField();
    return {
        send: async (email) => {
            const fromField = email.from || defaultFromField;

            console.log(yellowColorTemplate, '╔═══════════════════════╗');
            console.log(yellowColorTemplate, '║ Dummy email sender ✉️  ║');
            console.log(yellowColorTemplate, '╚═══════════════════════╝');
            console.log(`From:    ${fromField.name} <${fromField.email}>`);
            console.log(`To:      ${email.to}`);
            console.log(`Subject: ${email.subject}`);
            console.log(yellowColorTemplate, '═════════ Text ═════════');
            console.log(email.text);
            console.log(yellowColorTemplate, '═════════ HTML ═════════');
            console.log(email.html);
            console.log(yellowColorTemplate, '════════════════════════');

            return {
                success: true,
            };
        }
    }
}
