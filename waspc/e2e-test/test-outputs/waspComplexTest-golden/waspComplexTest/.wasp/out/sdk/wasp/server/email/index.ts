import { initEmailSender } from "./core/index.js";

const emailProvider = {
  type: "sendgrid",
  apiKey: process.env.SENDGRID_API_KEY,
} as const;

// PUBLIC API
export const emailSender = initEmailSender(emailProvider);

// PUBLIC API
export type { Email, EmailFromField } from "./core/types.js";
