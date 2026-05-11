import * as z from "zod";
import { ensureEnvSchema } from "../env/validation.js";
import { CompleteClientEnvSchema, clientEnvSchema } from "./env/schema.js";

// PUBLIC API
export const env: z.infer<CompleteClientEnvSchema> = ensureEnvSchema(
  import.meta.env,
  clientEnvSchema,
);
