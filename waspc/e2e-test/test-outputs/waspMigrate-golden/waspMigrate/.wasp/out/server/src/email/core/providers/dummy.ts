import { DummyEmailProvider, EmailSender } from "../types.js";
import { getDefaultFromField } from "../helpers.js";

const yellowColor = "\x1b[33m%s\x1b[0m";

export function initDummyEmailSender(
    config?: DummyEmailProvider,
): EmailSender {
    const defaultFromField = getDefaultFromField();
    return {
        send: async (email) => {
            const fromField = email.from || defaultFromField;
            // "Development email sender" in a unicode box for easy spotting in the console
            console.log(yellowColor, "╔══════════════════════════╗");
            console.log(yellowColor, "║ Development email sender ║");
            console.log(yellowColor, "╚══════════════════════════╝");
            console.log(yellowColor, `From:    ${fromField.name} <${fromField.email}>`);
            console.log(yellowColor, `To:      ${email.to}`);
            console.log(yellowColor, `Subject: ${email.subject}`);
            console.log(yellowColor, "--------------------------");
            console.log(email.text);
            console.log(yellowColor, "--------------------------");
            console.log(email.html);
            console.log(yellowColor, "--------------------------");
            return {
                success: true,
            };
        }
    }
}
