import * as z from 'zod';
import { ensureEnvSchema } from '../env/index.js';
const clientEnvSchema = z.object({
    REACT_APP_API_URL: z
        .string({
        required_error: 'REACT_APP_API_URL is required',
    })
        .default('http://localhost:3001')
});
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema);
//# sourceMappingURL=env.js.map