import * as z from "zod";
const userClientEnvSchema = z.object({});
const serverUrlSchema = z.string({
    error: 'REACT_APP_API_URL is required',
})
    .pipe(z.url({
    error: 'REACT_APP_API_URL must be a valid URL',
}));
const externalAuthProviderEnvSchema = z.object({});
const waspDevClientEnvSchema = z.object({
    "REACT_APP_API_URL": serverUrlSchema
        .default("http://localhost:3001"),
    ...externalAuthProviderEnvSchema.shape,
});
const waspProdClientEnvSchema = z.object({
    "REACT_APP_API_URL": serverUrlSchema,
    ...externalAuthProviderEnvSchema.shape,
});
const waspClientEnvSchema = import.meta.env.MODE === "production"
    ? waspProdClientEnvSchema
    : waspDevClientEnvSchema;
// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = z.object({
    ...userClientEnvSchema.shape,
    ...waspClientEnvSchema.shape
});
//# sourceMappingURL=schema.js.map