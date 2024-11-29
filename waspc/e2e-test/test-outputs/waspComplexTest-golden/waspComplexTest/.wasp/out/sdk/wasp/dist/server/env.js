import * as z from 'zod';
import { ensureEnvSchema } from '../env/index.js';
const serverCommonSchema = z.object({
    PORT: z.coerce.number().default(3001),
    DATABASE_URL: z.string({
        required_error: 'DATABASE_URL is required',
    }),
    PG_BOSS_NEW_OPTIONS: z.string().optional(),
    SENDGRID_API_KEY: z.string({
        required_error: 'SENDGRID_API_KEY is required',
    }),
    SKIP_EMAIL_VERIFICATION_IN_DEV: z
        .enum(['true', 'false'], {
        message: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
        .transform((value) => value === 'true')
        .default('false'),
    GOOGLE_CLIENT_ID: z.string({
        required_error: 'GOOGLE_CLIENT_ID is required',
    }),
    GOOGLE_CLIENT_SECRET: z.string({
        required_error: 'GOOGLE_CLIENT_SECRET is required',
    }),
});
const serverUrlSchema = z
    .string({
    required_error: 'WASP_SERVER_URL is required',
})
    .url({
    message: 'WASP_SERVER_URL must be a valid URL',
});
const clientUrlSchema = z
    .string({
    required_error: 'WASP_WEB_CLIENT_URL is required',
})
    .url({
    message: 'WASP_WEB_CLIENT_URL must be a valid URL',
});
const jwtTokenSchema = z
    .string({
    required_error: 'JWT_SECRET is required',
});
// In development, we provide default values for some environment variables
// to make the development process easier
const serverDevSchema = z.object({
    NODE_ENV: z.literal('development'),
    WASP_SERVER_URL: serverUrlSchema
        .default('http://localhost:3001'),
    WASP_WEB_CLIENT_URL: clientUrlSchema
        .default('http://localhost:3000/'),
    JWT_SECRET: jwtTokenSchema
        .default('DEVJWTSECRET'),
});
const serverProdSchema = z.object({
    NODE_ENV: z.literal('production'),
    WASP_SERVER_URL: serverUrlSchema,
    WASP_WEB_CLIENT_URL: clientUrlSchema,
    JWT_SECRET: jwtTokenSchema,
});
const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
    serverDevSchema.merge(serverCommonSchema),
    serverProdSchema.merge(serverCommonSchema)
]);
// PUBLIC API
export const env = ensureEnvSchema(process.env, serverEnvSchema);
//# sourceMappingURL=env.js.map