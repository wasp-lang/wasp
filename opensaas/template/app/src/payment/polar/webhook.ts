import { Subscription } from "@polar-sh/sdk/models/components/subscription.js";
import { SubscriptionStatus } from "@polar-sh/sdk/models/components/subscriptionstatus.js";
import { WebhookOrderPaidPayload } from "@polar-sh/sdk/models/components/webhookorderpaidpayload.js";
import { WebhookSubscriptionUpdatedPayload } from "@polar-sh/sdk/models/components/webhooksubscriptionupdatedpayload.js";
import { validateEvent } from "@polar-sh/sdk/webhooks";
import express from "express";
import { env, type MiddlewareConfigFn, type PrismaClient } from "wasp/server";
import type { PaymentsWebhook } from "wasp/server/api";
import { assertUnreachable } from "../../shared/utils";
import { UnhandledWebhookEventError } from "../errors";
import { getPaymentPlanIdByPaymentProcessorPlanId } from "../paymentProcessorPlans";
import {
  SubscriptionStatus as OpenSaasSubscriptionStatus,
  PaymentPlanId,
  paymentPlans,
} from "../plans";
import { updateUserCredits, updateUserSubscription } from "../user";

/**
 * Polar requires a raw request to construct events successfully.
 */
export const polarMiddlewareConfigFn: MiddlewareConfigFn = (
  middlewareConfig,
) => {
  middlewareConfig.delete("express.json");
  middlewareConfig.set(
    "express.raw",
    express.raw({ type: "application/json" }),
  );

  return middlewareConfig;
};

export const polarWebhook: PaymentsWebhook = async (
  request,
  response,
  context,
) => {
  const prismaUserDelegate = context.entities.User;
  try {
    const event = validateEvent(
      request.body,
      request.headers as Record<string, string>,
      env.POLAR_WEBHOOK_SECRET,
    );

    switch (event.type) {
      case "order.paid":
        await handleOrderPaid(event, prismaUserDelegate);
        break;
      case "subscription.updated":
        await handleSubscriptionUpdated(event, prismaUserDelegate);
        break;
      default:
        throw new UnhandledWebhookEventError(event.type);
    }
    return response.status(204).send();
  } catch (error) {
    if (error instanceof UnhandledWebhookEventError) {
      // In development, it is likely that we will receive events that we are not handling.
      if (process.env.NODE_ENV === "development") {
        console.info("Unhandled Polar webhook event in development: ", error);
      } else if (process.env.NODE_ENV === "production") {
        console.error("Unhandled Polar webhook event in production: ", error);
      }

      // We must return a 2XX status code, otherwise Polar will keep retrying the event.
      return response.status(200).json({ error: error.message });
    }

    console.error("Polar webhook error: ", error);
    if (error instanceof Error) {
      return response.status(400).json({ error: error.message });
    } else {
      return response
        .status(500)
        .json({ error: "Error processing Polar webhook event" });
    }
  }
};

async function handleOrderPaid(
  { data: order }: WebhookOrderPaidPayload,
  userDelegate: PrismaClient["user"],
): Promise<void> {
  const paymentPlanId = getPaymentPlanIdByPaymentProcessorPlanId(
    order.productId,
  );

  switch (paymentPlanId) {
    case PaymentPlanId.Credits10:
      await updateUserCredits(
        {
          paymentProcessorUserId: order.customerId,
          numOfCreditsPurchased: paymentPlans[paymentPlanId].effect.amount,
          datePaid: order.createdAt,
        },
        userDelegate,
      );
      break;
    case PaymentPlanId.Hobby:
    case PaymentPlanId.Pro:
      await updateUserSubscription(
        {
          paymentProcessorUserId: order.customerId,
          paymentPlanId,
          subscriptionStatus: OpenSaasSubscriptionStatus.Active,
          datePaid: order.createdAt,
        },
        userDelegate,
      );
      break;
    default:
      assertUnreachable(paymentPlanId);
  }
}

async function handleSubscriptionUpdated(
  { data: subscription }: WebhookSubscriptionUpdatedPayload,
  userDelegate: PrismaClient["user"],
): Promise<void> {
  const newSubscriptionStatus = getOpenSaasSubscriptionStatus(subscription);
  if (!newSubscriptionStatus) {
    return;
  }

  const paymentPlanId = getPaymentPlanIdByPaymentProcessorPlanId(
    subscription.productId,
  );

  await updateUserSubscription(
    {
      paymentProcessorUserId: subscription.customer.id,
      subscriptionStatus: newSubscriptionStatus,
      paymentPlanId,
    },
    userDelegate,
  );
}

function getOpenSaasSubscriptionStatus(
  subscription: Subscription,
): OpenSaasSubscriptionStatus | undefined {
  const polarToOpenSaasSubscriptionStatus: Record<
    SubscriptionStatus,
    OpenSaasSubscriptionStatus | undefined
  > = {
    trialing: OpenSaasSubscriptionStatus.Active,
    active: OpenSaasSubscriptionStatus.Active,
    past_due: OpenSaasSubscriptionStatus.PastDue,
    canceled: OpenSaasSubscriptionStatus.Deleted,
    unpaid: OpenSaasSubscriptionStatus.Deleted,
    incomplete_expired: OpenSaasSubscriptionStatus.Deleted,
    incomplete: undefined,
  };

  const subscriptionStatus =
    polarToOpenSaasSubscriptionStatus[subscription.status];

  if (
    subscriptionStatus === OpenSaasSubscriptionStatus.Active &&
    subscription.cancelAtPeriodEnd
  ) {
    return OpenSaasSubscriptionStatus.CancelAtPeriodEnd;
  }

  return subscriptionStatus;
}
