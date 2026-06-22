import * as z from "zod";
import { paymentPlansSchema } from "../env";

export const lemonSqueezyEnvSchema = paymentPlansSchema.extend({
  LEMONSQUEEZY_API_KEY: z.string({ error: "LEMONSQUEEZY_API_KEY is required" }),
  LEMONSQUEEZY_WEBHOOK_SECRET: z.string({
    error: "LEMONSQUEEZY_WEBHOOK_SECRET is required",
  }),
  LEMONSQUEEZY_STORE_ID: z.string({
    error: "LEMONSQUEEZY_STORE_ID is required",
  }),
});
