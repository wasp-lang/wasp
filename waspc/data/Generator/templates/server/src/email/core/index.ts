import { initSmtpEmailSender } from "./providers/smtp.js";

import type {
  EmailProvider,
  EmailProviderConstructor,
  EmailSender,
} from "./types.js";

const emailProviderConstructors: Record<
  EmailProvider["type"],
  EmailProviderConstructor
> = {
  smtp: initSmtpEmailSender,
};

export function initEmailSender(provider: EmailProvider): EmailSender {
  const init = emailProviderConstructors[provider.type];
  if (!init) {
    throw new Error(`Unsupported email provider: ${provider.type}`);
  }
  return init(provider);
}

export { SMTPEmailProvider } from "./types";
