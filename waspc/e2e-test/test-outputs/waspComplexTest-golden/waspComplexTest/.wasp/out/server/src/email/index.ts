import { initEmailSender } from "./core/index.js";

import waspServerConfig from '../config.js';
import { initDummyEmailSender } from "./core/providers/dummy.js";

const emailProvider = {
  type: "sendgrid",
  apiKey: process.env.SENDGRID_API_KEY,
} as const;

const areEmailsSentInDevelopment = process.env.SEND_EMAILS_IN_DEVELOPMENT === "true";
const isDummyEmailSenderUsed = waspServerConfig.isDevelopment && !areEmailsSentInDevelopment;

export const emailSender = isDummyEmailSenderUsed
  ? initDummyEmailSender()
  : initEmailSender(emailProvider);