{{={= =}=}}
import { env } from '../env.js';
import { initEmailSender } from "./core/index.js";
import { EmailSender } from "./core/types.js";

// TODO: We need to validate all the env variables
// For now, we are letting the runtime throw if they are not provided
{=# isSmtpProviderUsed =}
const emailProvider = { 
    type: "smtp",
    host: env.SMTP_HOST,
    port: env.SMTP_PORT,
    username: env.SMTP_USERNAME,
    password: env.SMTP_PASSWORD,
} as const;
{=/ isSmtpProviderUsed =}
{=# isSendGridProviderUsed =}
const emailProvider = {
  type: "sendgrid",
  apiKey: env.SENDGRID_API_KEY,
} as const;
{=/ isSendGridProviderUsed =}
{=# isMailgunProviderUsed =}
const emailProvider = {
  type: "mailgun",
  apiKey: env.MAILGUN_API_KEY,
  domain: env.MAILGUN_DOMAIN,
  apiUrl: env.MAILGUN_API_URL,
} as const;
{=/ isMailgunProviderUsed =}
{=# isDummyProviderUsed =}
const emailProvider = {
  type: "dummy",
} as const;
{=/ isDummyProviderUsed =}

// PUBLIC API
export const emailSender: EmailSender = initEmailSender(emailProvider);

// PUBLIC API
export type { Email, EmailFromField, EmailSender, SentMessageInfo } from "./core/types.js";
