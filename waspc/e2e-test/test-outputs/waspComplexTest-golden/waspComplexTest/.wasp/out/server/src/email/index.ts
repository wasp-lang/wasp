import { initEmailSender } from "./core/index.js";

const emailProvider = {
  type: "sendgrid",
  apiKey: process.env.SENDGRID_API_KEY,
} as const;

export const emailSender = initEmailSender(emailProvider);
