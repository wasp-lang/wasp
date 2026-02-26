export function initServer() {
  const required = ["STRIPE_API_KEY", "STRIPE_WEBHOOK_SECRET"];
  const missing = required.filter((key) => !process.env[key]);
  if (missing.length > 0) {
    console.warn(
      `[stripe-payments] Missing environment variables: ${missing.join(", ")}`,
    );
  }
  console.log("[stripe-payments] Server initialized");
}
