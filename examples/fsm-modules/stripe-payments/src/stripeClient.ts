import Stripe from "stripe";

if (!process.env.STRIPE_API_KEY) {
  throw new Error("Missing STRIPE_API_KEY environment variable");
}

export const stripe = new Stripe(process.env.STRIPE_API_KEY);
