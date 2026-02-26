import { Module } from "wasp-config";

export { SubscriptionStatus, isActiveSubscription } from "./types.js";

export const PACKAGE_NAME = "@waspello/stripe-payments";

export type StripePaymentsConfig = {
  userEntityName: string;
  premiumPlanPriceId: string;
  subscriptionRoute: string;
  webhookRoute?: string;
  successUrl?: string;
  cancelUrl?: string;
};

export function createStripePaymentsModule(
  config: StripePaymentsConfig,
): Module {
  const mod = new Module(PACKAGE_NAME);
  const {
    userEntityName,
    premiumPlanPriceId,
    subscriptionRoute,
    webhookRoute = "/payments-webhook",
    successUrl = `${subscriptionRoute}?status=success`,
    cancelUrl = `${subscriptionRoute}?status=canceled`,
  } = config;

  mod.provide("userEntityName", userEntityName);
  mod.provide("premiumPlanPriceId", premiumPlanPriceId);
  mod.provide("subscriptionRoute", subscriptionRoute);
  mod.provide("webhookRoute", webhookRoute);
  mod.provide("successUrl", successUrl);
  mod.provide("cancelUrl", cancelUrl);

  const entities = [userEntityName];

  // Page + Route
  const subscriptionPage = mod.page("SubscriptionPage", {
    component: {
      importDefault: "SubscriptionPage",
      from: "@src/SubscriptionPage",
    },
    authRequired: true,
  });
  mod.route("SubscriptionRoute", {
    path: subscriptionRoute,
    to: subscriptionPage,
  });

  // Query
  mod.query("getSubscriptionStatus", {
    fn: { import: "getSubscriptionStatus", from: "@src/queries" },
    entities,
    auth: true,
  });

  // Actions
  mod.action("generateCheckoutSession", {
    fn: { import: "generateCheckoutSession", from: "@src/actions" },
    entities,
    auth: true,
  });

  mod.action("cancelSubscription", {
    fn: { import: "cancelSubscription", from: "@src/actions" },
    entities,
    auth: true,
  });

  mod.action("getCustomerPortalUrl", {
    fn: { import: "getCustomerPortalUrl", from: "@src/actions" },
    entities,
    auth: true,
  });

  // Webhook API
  mod.api("stripeWebhook", {
    fn: { import: "stripeWebhook", from: "@src/webhook" },
    httpRoute: { method: "POST", route: webhookRoute },
    entities,
    auth: false,
  });

  mod.apiNamespace("stripeWebhookNamespace", {
    middlewareConfigFn: {
      import: "webhookMiddleware",
      from: "@src/middleware",
    },
    path: webhookRoute,
  });

  // Setup hooks
  mod.serverSetup({ import: "initServer", from: "@src/serverSetup" });
  mod.clientSetup({ import: "initClient", from: "@src/clientSetup" });

  return mod;
}
