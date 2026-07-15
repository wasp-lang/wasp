import * as z from "zod";
import { ensureEnvSchema } from "../env/validation";
import { CompleteClientEnvSchema, clientEnvSchema } from "./env/schema";

// PUBLIC API
export const env: z.infer<CompleteClientEnvSchema> = ensureEnvSchema(
  import.meta.env,
  clientEnvSchema,
);
