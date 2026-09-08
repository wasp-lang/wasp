import type * as z from "zod";
import { ensureEnvSchema } from "../env/validation";
import { type CompleteClientEnvSchema, clientEnvSchema } from "./env/schema";

// PUBLIC API
export const env: z.infer<CompleteClientEnvSchema> = ensureEnvSchema(
  import.meta.env,
  clientEnvSchema,
);
