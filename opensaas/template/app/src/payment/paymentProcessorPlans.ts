import { env } from "wasp/server";
import { type PaymentPlan, PaymentPlanId } from "./plans";

/**
 * The ID under which this payment plan is identified on your payment processor.
 *
 * E.g. price id on Stripe, or variant id on LemonSqueezy.
 */
export const paymentProcessorPlanIds = {
  [PaymentPlanId.Hobby]: env.PAYMENTS_HOBBY_SUBSCRIPTION_PLAN_ID,
  [PaymentPlanId.Pro]: env.PAYMENTS_PRO_SUBSCRIPTION_PLAN_ID,
  [PaymentPlanId.Credits10]: env.PAYMENTS_CREDITS_10_PLAN_ID,
} as const satisfies Record<PaymentPlanId, string>;

/**
 * Returns your payment processor plan ID for a given Open SaaS `PaymentPlan`.
 */
export function getPaymentProcessorPlanId(paymentPlan: PaymentPlan): string {
  return paymentProcessorPlanIds[paymentPlan.id];
}

/**
 * Returns Open SaaS `PaymentPlanId` for some payment provider's plan ID.
 *
 * Different payment providers track plan ID in different ways.
 * e.g. Stripe price ID, Polar product ID...
 */
export function getPaymentPlanIdByPaymentProcessorPlanId(
  paymentProcessorPlanId: string,
): PaymentPlanId {
  for (const [planId, processorPlanId] of Object.entries(
    paymentProcessorPlanIds,
  )) {
    if (processorPlanId === paymentProcessorPlanId) {
      return planId as PaymentPlanId;
    }
  }

  throw new Error(
    `Unknown payment processor plan ID: ${paymentProcessorPlanId}`,
  );
}
