import { action, api, page, query, route, type Spec } from "@wasp.sh/spec";

import { CheckoutResultPage } from "./CheckoutResultPage" with { type: "ref" };
import {
  generateCheckoutSession,
  getCustomerPortalUrl,
} from "./operations" with { type: "ref" };
import { PricingPage } from "./PricingPage" with { type: "ref" };
import {
  paymentsMiddlewareConfigFn,
  paymentsWebhook,
} from "./webhook" with { type: "ref" };

export const paymentSpec: Spec = [
  route("PricingPageRoute", "/pricing", page(PricingPage), { prerender: true }),
  route(
    "CheckoutResultRoute",
    "/checkout",
    page(CheckoutResultPage, { authRequired: true }),
  ),
  query(getCustomerPortalUrl, { entities: ["User"] }),
  action(generateCheckoutSession, { entities: ["User"] }),
  api("POST", "/payments-webhook", paymentsWebhook, {
    entities: ["User"],
    middlewareConfigFn: paymentsMiddlewareConfigFn,
  }),
];
