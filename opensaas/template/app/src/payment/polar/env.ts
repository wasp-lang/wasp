import * as z from "zod";
import { paymentPlansSchema } from "../env";

export const polarEnvSchema = paymentPlansSchema.extend({
  POLAR_ORGANIZATION_ACCESS_TOKEN: z.string({
    error: "POLAR_ORGANIZATION_ACCESS_TOKEN is required",
  }),
  POLAR_SANDBOX_MODE: z.string({ error: "POLAR_SANDBOX_MODE is required" }),
  POLAR_WEBHOOK_SECRET: z.string({ error: "POLAR_WEBHOOK_SECRET is required" }),
});
