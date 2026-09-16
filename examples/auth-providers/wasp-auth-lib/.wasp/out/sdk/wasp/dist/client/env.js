import { ensureEnvSchema } from "../env/validation";
import { clientEnvSchema } from "./env/schema";
// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema);
//# sourceMappingURL=env.js.map