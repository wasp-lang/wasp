export class UnhandledWebhookEventError extends Error {
  constructor(eventType: string) {
    super(`Unhandled event type: ${eventType}`);
    this.name = "UnhandledWebhookEventError";
  }
}
