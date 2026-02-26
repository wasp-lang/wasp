import { HttpError } from "wasp/server";
import type { AuthenticatedActionDefinition } from "wasp/server/module";
import { requireUser } from "./auth.js";
import {
  createCheckoutSession,
  ensureStripeCustomer,
  getCustomerPortalSession,
} from "./checkoutUtils.js";
import { moduleConfig } from "./config.js";

const { userEntityName, premiumPlanPriceId } = moduleConfig;

export const generateCheckoutSession: AuthenticatedActionDefinition<void, { sessionUrl: string | null }> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  const dbUser = await context.entities[userEntityName].findUniqueOrThrow({
    where: { id: user.id },
  });

  const email = dbUser.email ?? user.identities.email?.id;
  if (!email) {
    throw new HttpError(403, "User must have an email to subscribe");
  }

  const customer = await ensureStripeCustomer(email);

  await context.entities[userEntityName].update({
    where: { id: user.id },
    data: { stripeCustomerId: customer.id },
  });

  const session = await createCheckoutSession(customer.id, premiumPlanPriceId);
  return { sessionUrl: session.url };
};

export const cancelSubscription: AuthenticatedActionDefinition<void, { success: boolean }> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  const dbUser = await context.entities[userEntityName].findUniqueOrThrow({
    where: { id: user.id },
  });

  const stripeCustomerId = (dbUser as any).stripeCustomerId;
  if (!stripeCustomerId) {
    throw new HttpError(400, "No Stripe customer found");
  }

  const { stripe } = await import("./stripeClient.js");
  const subscriptions = await stripe.subscriptions.list({
    customer: stripeCustomerId,
    status: "active",
  });

  for (const sub of subscriptions.data) {
    await stripe.subscriptions.update(sub.id, {
      cancel_at_period_end: true,
    });
  }

  await context.entities[userEntityName].update({
    where: { id: user.id },
    data: { subscriptionStatus: "cancel_at_period_end" },
  });

  return { success: true };
};

export const getCustomerPortalUrl: AuthenticatedActionDefinition<void, { portalUrl: string | null }> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  const dbUser = await context.entities[userEntityName].findUniqueOrThrow({
    where: { id: user.id },
  });

  const stripeCustomerId = (dbUser as any).stripeCustomerId;
  if (!stripeCustomerId) {
    throw new HttpError(400, "No Stripe customer found");
  }

  const session = await getCustomerPortalSession(stripeCustomerId);
  return { portalUrl: session.url };
};
