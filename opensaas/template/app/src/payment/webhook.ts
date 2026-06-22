import { paymentProcessor } from "./paymentProcessor";

export const paymentsWebhook = paymentProcessor.webhook;
export const paymentsMiddlewareConfigFn =
  paymentProcessor.webhookMiddlewareConfigFn;
