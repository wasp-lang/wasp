import { ensureEnvSchema } from "../env/validation.js";
import { getClientEnvSchema } from "./env/schema.js";

// PUBLIC API
export const env = ensureEnvSchema(
  import.meta.env,
  getClientEnvSchema(import.meta.env.MODE),
);
