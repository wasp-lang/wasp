import * as z from "zod";
import { ensureEnvSchema } from "../env/validation.js";
import { ClientEnvSchema, clientEnvSchema } from "./env/schema.js";

// PUBLIC API
export const env: z.infer<ClientEnvSchema> = ensureEnvSchema(
  import.meta.env,
  clientEnvSchema,
);
