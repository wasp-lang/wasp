{{={= =}=}}

import { initEmailSender } from "./core/index.js";

{=# isSmtpProviderUsed =}
const emailProvider = {
    type: "smtp",
    host: process.env.SMTP_HOST,
    port: parseInt(process.env.SMTP_PORT, 10),
    username: process.env.SMTP_USERNAME,
    password: process.env.SMTP_PASSWORD,
} as const;
{=/ isSmtpProviderUsed =}
{=# isSendGridProviderUsed =}
const emailProvider = {
  type: "sendgrid",
  apiKey: process.env.SENDGRID_API_KEY,
} as const;
{=/ isSendGridProviderUsed =}

export const emailSender = initEmailSender(emailProvider);
