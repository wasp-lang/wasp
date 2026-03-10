import * as z from "zod";
import { ensureEnvSchema } from "../env/validation.js";
import { clientEnvSchema, userClientEnvSchema } from "./env/schema.js";

const _env = ensureEnvSchema(import.meta.env, clientEnvSchema);

// PUBLIC API
export const env: typeof _env & z.infer<typeof userClientEnvSchema> = _env;
