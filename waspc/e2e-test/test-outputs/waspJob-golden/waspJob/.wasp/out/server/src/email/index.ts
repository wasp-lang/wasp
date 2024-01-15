import { initEmailSender } from "./core/index.js";

const emailProvider = {
  type: "dummy",
} as const;

export const emailSender = initEmailSender(emailProvider);
