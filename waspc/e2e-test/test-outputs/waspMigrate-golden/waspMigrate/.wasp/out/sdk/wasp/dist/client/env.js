import { ensureEnvSchema } from '../env/index.js';
import { clientEnvSchema } from './env/schema.js';
// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema);
//# sourceMappingURL=env.js.map