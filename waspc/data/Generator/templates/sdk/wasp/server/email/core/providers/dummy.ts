import { getDefaultFromField } from "../helpers.js";
import { DummyEmailProvider, EmailSender } from "../types";

import { colorize } from "wasp/universal/ansiColors";

// PRIVATE API
export function initDummyEmailSender(
  _config?: DummyEmailProvider,
): EmailSender {
  const defaultFromField = getDefaultFromField();
  return {
    send: async (email) => {
      const fromField = email.from || defaultFromField;

      console.log(colorize("yellow", "╔═══════════════════════╗"));
      console.log(colorize("yellow", "║ Dummy email sender ✉️  ║"));
      console.log(colorize("yellow", "╚═══════════════════════╝"));
      console.log(`From:    ${fromField.name} <${fromField.email}>`);
      console.log(`To:      ${email.to}`);
      console.log(`Subject: ${email.subject}`);
      console.log(colorize("yellow", "═════════ Text ═════════"));
      console.log(email.text);
      console.log(colorize("yellow", "═════════ HTML ═════════"));
      console.log(email.html);
      console.log(colorize("yellow", "════════════════════════"));

      return {
        success: true,
      };
    },
  };
}
