import * as z from "zod";
import { ensureEnvSchema } from "../env/validation";
const userServerEnvSchema = z.object({});
const waspCommonServerEnvSchema = z.object({
    PORT: z.coerce.number().default(3001),
    DATABASE_URL: z.string({
        error: 'DATABASE_URL is required',
    }),
    PG_BOSS_NEW_OPTIONS: z.string().optional(),
    SKIP_EMAIL_VERIFICATION_IN_DEV: z
        .enum(['true', 'false'], {
        error: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
        .default('false')
        .transform((value) => value === 'true'),
    "WASP_AUTH_GOOGLE_CLIENT_ID": z.string({
        error: "WASP_AUTH_GOOGLE_CLIENT_ID is required by the 'external:wasp-auth' auth provider: Google OAuth client id (framework names like GOOGLE_CLIENT_ID are reserved for Wasp itself).",
    }),
    "WASP_AUTH_GOOGLE_CLIENT_SECRET": z.string({
        error: "WASP_AUTH_GOOGLE_CLIENT_SECRET is required by the 'external:wasp-auth' auth provider: Google OAuth client secret.",
    }),
});
const serverUrlSchema = z.string({
    error: 'WASP_SERVER_URL is required',
})
    .pipe(z.url({
    error: 'WASP_SERVER_URL must be a valid URL',
}));
const clientUrlSchema = z.string({
    error: 'WASP_WEB_CLIENT_URL is required',
})
    .pipe(z.url({
    error: 'WASP_WEB_CLIENT_URL must be a valid URL',
}));
// In development, we provide default values for some environment variables
// to make the development process easier.
const waspDevServerEnvSchema = z.object({
    NODE_ENV: z.literal("development"),
    "WASP_SERVER_URL": serverUrlSchema
        .default("http://localhost:3001"),
    "WASP_WEB_CLIENT_URL": clientUrlSchema
        .default("http://localhost:3000/"),
    "WASP_AUTH_TOKENS_SECRET": z.string({
        error: "WASP_AUTH_TOKENS_SECRET is required by the 'external:wasp-auth' auth provider: Signs email verification links, password reset links and OAuth one-time codes. Required in production, defaulted in development.",
    }).default("DEV_WASP_AUTH_TOKENS_SECRET"),
});
const waspProdServerEnvSchema = z.object({
    NODE_ENV: z.literal("production"),
    "WASP_SERVER_URL": serverUrlSchema,
    "WASP_WEB_CLIENT_URL": clientUrlSchema,
    "WASP_AUTH_TOKENS_SECRET": z.string({
        error: "WASP_AUTH_TOKENS_SECRET is required by the 'external:wasp-auth' auth provider: Signs email verification links, password reset links and OAuth one-time codes. Required in production, defaulted in development.",
    }),
});
const waspServerEnvSchema = z.discriminatedUnion("NODE_ENV", [
    z.object({ ...waspCommonServerEnvSchema.shape, ...waspDevServerEnvSchema.shape }),
    z.object({ ...waspCommonServerEnvSchema.shape, ...waspProdServerEnvSchema.shape }),
]);
const serverEnvSchema = userServerEnvSchema.and(waspServerEnvSchema);
const defaultNodeEnvValue = waspDevServerEnvSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;
// PUBLIC API
export const env = ensureEnvSchema({
    NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
    ...restEnv,
}, serverEnvSchema);
function getRequiredEnvVarErrorMessage(featureName, envVarName) {
    return `${envVarName} is required when using ${featureName}`;
}
//# sourceMappingURL=env.js.map