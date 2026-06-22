import * as z from "zod";

export const paymentPlansSchema = z.object({
  PAYMENTS_HOBBY_SUBSCRIPTION_PLAN_ID: z.string({
    error: "PAYMENTS_HOBBY_SUBSCRIPTION_PLAN_ID is required",
  }),
  PAYMENTS_PRO_SUBSCRIPTION_PLAN_ID: z.string({
    error: "PAYMENTS_PRO_SUBSCRIPTION_PLAN_ID is required",
  }),
  PAYMENTS_CREDITS_10_PLAN_ID: z.string({
    error: "PAYMENTS_CREDITS_10_PLAN_ID is required",
  }),
});
