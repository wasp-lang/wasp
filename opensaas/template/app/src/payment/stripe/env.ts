import * as z from "zod";
import { paymentPlansSchema } from "../env";

export const stripeEnvSchema = paymentPlansSchema.extend({
  STRIPE_API_KEY: z.string({ error: "STRIPE_API_KEY is required" }),
  STRIPE_WEBHOOK_SECRET: z.string({
    error: "STRIPE_WEBHOOK_SECRET is required",
  }),
});
