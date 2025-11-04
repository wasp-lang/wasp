import { env } from '../env.js';
import { initEmailSender } from "./core/index.js";
import { EmailSender } from "./core/types.js";

const emailProvider = { 
    type: "smtp",
    host: env.SMTP_HOST,
    port: env.SMTP_PORT,
    username: env.SMTP_USERNAME,
    password: env.SMTP_PASSWORD,
} as const;

// PUBLIC API
export const emailSender: EmailSender = initEmailSender(emailProvider);

// PUBLIC API
export type { Email, EmailFromField, EmailSender, SentMessageInfo } from "./core/types.js";
