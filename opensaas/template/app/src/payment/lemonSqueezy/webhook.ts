import { getCustomer } from "@lemonsqueezy/lemonsqueezy.js";
import { type PrismaClient } from "@prisma/client";
import crypto from "crypto";
import express from "express";
import { env, HttpError, type MiddlewareConfigFn } from "wasp/server";
import { type PaymentsWebhook } from "wasp/server/api";
import { assertUnreachable } from "../../shared/utils";
import { UnhandledWebhookEventError } from "../errors";
import { getPaymentProcessorPlanId } from "../paymentProcessorPlans";
import { PaymentPlanId, paymentPlans, SubscriptionStatus } from "../plans";
import { updateUserLemonSqueezyPaymentDetails } from "./paymentDetails";
import {
  parseWebhookPayload,
  type OrderData,
  type SubscriptionData,
} from "./webhookPayload";

export const lemonSqueezyWebhook: PaymentsWebhook = async (
  request,
  response,
  context,
) => {
  try {
    const rawRequestBody = parseRequestBody(request);

    const { eventName, meta, data } = await parseWebhookPayload(rawRequestBody);
    const userId = meta.custom_data.user_id;
    const prismaUserDelegate = context.entities.User;

    switch (eventName) {
      case "order_created":
        await handleOrderCreated(data, userId, prismaUserDelegate);
        break;
      case "subscription_created":
        await handleSubscriptionCreated(data, userId, prismaUserDelegate);
        break;
      case "subscription_updated":
        await handleSubscriptionUpdated(data, userId, prismaUserDelegate);
        break;
      case "subscription_cancelled":
        await handleSubscriptionCancelled(data, userId, prismaUserDelegate);
        break;
      case "subscription_expired":
        await handleSubscriptionExpired(data, userId, prismaUserDelegate);
        break;
      default:
        // If you'd like to handle more events, you can add more cases above.
        assertUnreachable(eventName);
    }

    return response.status(200).json({ received: true });
  } catch (err) {
    if (err instanceof UnhandledWebhookEventError) {
      console.error(err.message);
      return response.status(422).json({ error: err.message });
    }

    console.error("Webhook error:", err);
    if (err instanceof HttpError) {
      return response.status(err.statusCode).json({ error: err.message });
    } else {
      return response
        .status(400)
        .json({ error: "Error Processing Lemon Squeezy Webhook Event" });
    }
  }
};

function parseRequestBody(request: express.Request): string {
  const requestBody = request.body.toString("utf8");
  const signature = request.get("X-Signature");
  if (!signature) {
    throw new HttpError(400, "Lemon Squeezy webhook signature not provided");
  }

  const secret = env.LEMONSQUEEZY_WEBHOOK_SECRET;
  const hmac = crypto.createHmac("sha256", secret);
  const digest = Buffer.from(hmac.update(requestBody).digest("hex"), "utf8");

  if (!crypto.timingSafeEqual(Buffer.from(signature, "utf8"), digest)) {
    throw new HttpError(400, "Invalid signature");
  }

  return requestBody;
}

export const lemonSqueezyMiddlewareConfigFn: MiddlewareConfigFn = (
  middlewareConfig,
) => {
  // We need to delete the default 'express.json' middleware and replace it with 'express.raw' middleware
  // because webhook data in the body of the request as raw JSON, not as JSON in the body of the request.
  middlewareConfig.delete("express.json");
  middlewareConfig.set(
    "express.raw",
    express.raw({ type: "application/json" }),
  );
  return middlewareConfig;
};

// This will fire for one-time payment orders AND subscriptions. But subscriptions will ALSO send a follow-up
// event of 'subscription_created'. So we use this handler mainly to process one-time, credit-based orders,
// as well as to save the customer portal URL and customer id for the user.
async function handleOrderCreated(
  data: OrderData,
  userId: string,
  prismaUserDelegate: PrismaClient["user"],
) {
  const { customer_id, status, first_order_item, order_number } =
    data.attributes;
  const lemonSqueezyId = customer_id.toString();

  const planId = getPlanIdByVariantId(first_order_item.variant_id.toString());
  const plan = paymentPlans[planId];

  const lemonSqueezyCustomerPortalUrl = await fetchUserCustomerPortalUrl({
    lemonSqueezyId,
  });

  let numOfCreditsPurchased: number | undefined = undefined;
  let datePaid: Date | undefined = undefined;
  if (status === "paid" && plan.effect.kind === "credits") {
    numOfCreditsPurchased = plan.effect.amount;
    datePaid = new Date();
  }

  await updateUserLemonSqueezyPaymentDetails(
    {
      lemonSqueezyId,
      userId,
      lemonSqueezyCustomerPortalUrl,
      numOfCreditsPurchased,
      datePaid,
    },
    prismaUserDelegate,
  );

  console.log(`Order ${order_number} created for user ${lemonSqueezyId}`);
}

async function handleSubscriptionCreated(
  data: SubscriptionData,
  userId: string,
  prismaUserDelegate: PrismaClient["user"],
) {
  const { customer_id, status, variant_id } = data.attributes;
  const lemonSqueezyId = customer_id.toString();

  const planId = getPlanIdByVariantId(variant_id.toString());

  if (status === "active") {
    await updateUserLemonSqueezyPaymentDetails(
      {
        lemonSqueezyId,
        userId,
        subscriptionPlan: planId,
        subscriptionStatus: status as SubscriptionStatus,
        datePaid: new Date(),
      },
      prismaUserDelegate,
    );
  } else {
    console.warn(
      `Unexpected status '${status}' for newly created subscription`,
    );
  }

  console.log(`Subscription created for user ${lemonSqueezyId}`);
}

// NOTE: LemonSqueezy's 'subscription_updated' event is sent as a catch-all and fires even after 'subscription_created' & 'order_created'.
async function handleSubscriptionUpdated(
  data: SubscriptionData,
  userId: string,
  prismaUserDelegate: PrismaClient["user"],
) {
  const { customer_id, status, variant_id } = data.attributes;
  const lemonSqueezyId = customer_id.toString();

  const planId = getPlanIdByVariantId(variant_id.toString());

  // We ignore other statuses like 'paused' and 'unpaid' for now, because we block user usage if their status is NOT active.
  // Note that a status changes to 'past_due' on a failed payment retry, then after 4 unsuccesful payment retries status
  // becomes 'unpaid' and finally 'expired' (i.e. 'deleted').
  // NOTE: ability to pause or trial a subscription is something that has to be additionally configured in the lemon squeezy dashboard.
  // If you do enable these features, make sure to handle these statuses here.
  if (status === "past_due" || status === "active") {
    await updateUserLemonSqueezyPaymentDetails(
      {
        lemonSqueezyId,
        userId,
        subscriptionPlan: planId,
        subscriptionStatus: status as SubscriptionStatus,
        ...(status === "active" && { datePaid: new Date() }),
      },
      prismaUserDelegate,
    );
    console.log(`Subscription updated for user ${lemonSqueezyId}`);
  }
}

async function handleSubscriptionCancelled(
  data: SubscriptionData,
  userId: string,
  prismaUserDelegate: PrismaClient["user"],
) {
  const { customer_id } = data.attributes;
  const lemonSqueezyId = customer_id.toString();

  await updateUserLemonSqueezyPaymentDetails(
    {
      lemonSqueezyId,
      userId,
      // cancel_at_period_end is the Stripe equivalent of LemonSqueezy's cancelled
      subscriptionStatus: "cancel_at_period_end" as SubscriptionStatus,
    },
    prismaUserDelegate,
  );

  console.log(`Subscription cancelled for user ${lemonSqueezyId}`);
}

async function handleSubscriptionExpired(
  data: SubscriptionData,
  userId: string,
  prismaUserDelegate: PrismaClient["user"],
) {
  const { customer_id } = data.attributes;
  const lemonSqueezyId = customer_id.toString();

  await updateUserLemonSqueezyPaymentDetails(
    {
      lemonSqueezyId,
      userId,
      // deleted is the Stripe equivalent of LemonSqueezy's expired
      subscriptionStatus: SubscriptionStatus.Deleted,
    },
    prismaUserDelegate,
  );

  console.log(`Subscription expired for user ${lemonSqueezyId}`);
}

async function fetchUserCustomerPortalUrl({
  lemonSqueezyId,
}: {
  lemonSqueezyId: string;
}): Promise<string> {
  const { data: lemonSqueezyCustomer, error } =
    await getCustomer(lemonSqueezyId);
  if (error) {
    throw new Error(
      `Error fetching customer portal URL for user lemonsqueezy id ${lemonSqueezyId}: ${error}`,
    );
  }
  const customerPortalUrl =
    lemonSqueezyCustomer.data.attributes.urls.customer_portal;
  if (!customerPortalUrl) {
    throw new Error(
      `No customer portal URL found for user lemonsqueezy id ${lemonSqueezyId}`,
    );
  }
  return customerPortalUrl;
}

function getPlanIdByVariantId(variantId: string): PaymentPlanId {
  const planId = Object.values(PaymentPlanId).find(
    (planId) => getPaymentProcessorPlanId(paymentPlans[planId]) === variantId,
  );
  if (!planId) {
    throw new Error(`No plan with LemonSqueezy variant id ${variantId}`);
  }
  return planId;
}
