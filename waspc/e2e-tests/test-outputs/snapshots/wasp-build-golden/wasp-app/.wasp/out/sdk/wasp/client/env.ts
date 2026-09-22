import type * as z from "zod";
import { ensureEnvSchema } from "@wasp.sh/lib-sdk-core";
import { type CompleteClientEnvSchema, clientEnvSchema } from "./env/schema";

// PUBLIC API
export const env: z.infer<CompleteClientEnvSchema> = ensureEnvSchema(
  import.meta.env,
  clientEnvSchema,
);
