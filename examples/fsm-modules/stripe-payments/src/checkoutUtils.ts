import type Stripe from "stripe";
import { config } from "wasp/server";
import { stripe } from "./stripeClient.js";
import { moduleConfig } from "./config.js";

export async function ensureStripeCustomer(
  email: string,
): Promise<Stripe.Customer> {
  const customers = await stripe.customers.list({ email });
  if (customers.data.length > 0) {
    return customers.data[0];
  }
  return stripe.customers.create({ email });
}

export function createCheckoutSession(
  customerId: string,
  priceId: string,
): Promise<Stripe.Checkout.Session> {
  return stripe.checkout.sessions.create({
    customer: customerId,
    line_items: [{ price: priceId, quantity: 1 }],
    mode: "subscription",
    success_url: `${config.frontendUrl}${moduleConfig.successUrl}`,
    cancel_url: `${config.frontendUrl}${moduleConfig.cancelUrl}`,
    automatic_tax: { enabled: false },
  });
}

export async function getCustomerPortalSession(
  stripeCustomerId: string,
): Promise<Stripe.BillingPortal.Session> {
  return stripe.billingPortal.sessions.create({
    customer: stripeCustomerId,
  });
}
