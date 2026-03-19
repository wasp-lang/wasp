
import * as z from "zod";
import { ensureEnvSchema } from "../env/validation.js";
import { getClientEnvSchema, userClientEnvSchema } from "./env/schema.js";

const _env = ensureEnvSchema(
  import.meta.env, 
  getClientEnvSchema(import.meta.env.MODE)
);

// PUBLIC API
export const env: typeof _env & z.infer<typeof userClientEnvSchema> = _env;
