{{={= =}=}}
import { env } from '../env.js';
import { initEmailSender } from "./core/index.js";
import { EmailSender } from "./core/types.js";

{=# isSmtpProviderEnabled =}
const emailProvider = { 
    type: "smtp",
    host: env.SMTP_HOST,
    port: env.SMTP_PORT,
    username: env.SMTP_USERNAME,
    password: env.SMTP_PASSWORD,
} as const;
{=/ isSmtpProviderEnabled =}
{=# isSendGridProviderEnabled =}
const emailProvider = {
  type: "sendgrid",
  apiKey: env.SENDGRID_API_KEY,
} as const;
{=/ isSendGridProviderEnabled =}
{=# isMailgunProviderEnabled =}
const emailProvider = {
  type: "mailgun",
  apiKey: env.MAILGUN_API_KEY,
  domain: env.MAILGUN_DOMAIN,
  apiUrl: env.MAILGUN_API_URL,
} as const;
{=/ isMailgunProviderEnabled =}
{=# isDummyProviderEnabled =}
const emailProvider = {
  type: "dummy",
} as const;
{=/ isDummyProviderEnabled =}

// PUBLIC API
export const emailSender: EmailSender = initEmailSender(emailProvider);

// PUBLIC API
export type { Email, EmailFromField, EmailSender, SentMessageInfo } from "./core/types.js";
