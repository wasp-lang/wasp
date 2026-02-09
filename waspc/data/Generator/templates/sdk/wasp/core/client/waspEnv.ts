import { ensureEnvSchema } from 'wasp/env/validation';
import { waspClientEnvSchema } from "./env/waspSchema";

export const waspEnv = ensureEnvSchema(import.meta.env, waspClientEnvSchema)
