import type { Request, Response } from "express";
import type Stripe from "stripe";
import type { OperationContext } from "wasp/server/module";
import { stripe } from "./stripeClient.js";
import { moduleConfig } from "./config.js";
import { SubscriptionStatus } from "./types.js";

const { userEntityName } = moduleConfig;

export async function stripeWebhook(
  req: Request,
  res: Response,
  context: OperationContext,
) {
  try {
    const event = constructEvent(req);

    switch (event.type) {
      case "invoice.paid":
        await handleInvoicePaid(event, context);
        break;
      case "customer.subscription.updated":
        await handleSubscriptionUpdated(event, context);
        break;
      case "customer.subscription.deleted":
        await handleSubscriptionDeleted(event, context);
        break;
      default:
        console.info(`Unhandled Stripe event type: ${event.type}`);
    }

    return res.status(204).send();
  } catch (error) {
    console.error("Stripe webhook error:", error);
    const message =
      error instanceof Error ? error.message : "Webhook processing failed";
    return res.status(400).json({ error: message });
  }
}

function constructEvent(req: Request): Stripe.Event {
  const secret = process.env.STRIPE_WEBHOOK_SECRET;
  if (!secret) {
    throw new Error("Missing STRIPE_WEBHOOK_SECRET environment variable");
  }

  const signature = req.headers["stripe-signature"];
  if (!signature) {
    throw new Error("Missing stripe-signature header");
  }

  return stripe.webhooks.constructEvent(req.body, signature, secret);
}

function getCustomerId(
  customer: string | Stripe.Customer | Stripe.DeletedCustomer | null,
): string {
  if (!customer) {
    throw new Error("Customer is missing from event");
  }
  return typeof customer === "string" ? customer : customer.id;
}

async function handleInvoicePaid(
  event: Stripe.InvoicePaidEvent,
  context: OperationContext,
) {
  const invoice = event.data.object;
  const customerId = getCustomerId(invoice.customer);
  const paidAt = invoice.status_transitions.paid_at;
  const datePaid = paidAt ? new Date(paidAt * 1000) : new Date();

  await context.entities[userEntityName].updateMany({
    where: { stripeCustomerId: customerId },
    data: {
      subscriptionStatus: SubscriptionStatus.Active,
      subscriptionPlan: "premium",
      datePaid,
    },
  });
}

async function handleSubscriptionUpdated(
  event: Stripe.CustomerSubscriptionUpdatedEvent,
  context: OperationContext,
) {
  const subscription = event.data.object;
  const customerId = getCustomerId(subscription.customer);
  const status = mapSubscriptionStatus(subscription);

  if (!status) return;

  await context.entities[userEntityName].updateMany({
    where: { stripeCustomerId: customerId },
    data: { subscriptionStatus: status },
  });
}

async function handleSubscriptionDeleted(
  event: Stripe.CustomerSubscriptionDeletedEvent,
  context: OperationContext,
) {
  const subscription = event.data.object;
  const customerId = getCustomerId(subscription.customer);

  await context.entities[userEntityName].updateMany({
    where: { stripeCustomerId: customerId },
    data: {
      subscriptionStatus: SubscriptionStatus.Deleted,
      subscriptionPlan: null,
    },
  });
}

function mapSubscriptionStatus(
  subscription: Stripe.Subscription,
): SubscriptionStatus | undefined {
  const mapping: Record<Stripe.Subscription.Status, SubscriptionStatus | undefined> = {
    trialing: SubscriptionStatus.Active,
    active: SubscriptionStatus.Active,
    past_due: SubscriptionStatus.PastDue,
    canceled: SubscriptionStatus.Deleted,
    unpaid: SubscriptionStatus.Deleted,
    incomplete_expired: SubscriptionStatus.Deleted,
    paused: undefined,
    incomplete: undefined,
  };

  const status = mapping[subscription.status];
  if (status === SubscriptionStatus.Active && subscription.cancel_at_period_end) {
    return SubscriptionStatus.CancelAtPeriodEnd;
  }
  return status;
}
