/**
 * Helpers for validating your app's environment variables.
 *
 * @module
 */

/**
 * Defines a Zod schema to validate your client or server env vars.
 *
 * It returns the schema unchanged, and only exists to make sure the schema is
 * type-checked. Export the schema from a file in `src/` and pass it to the
 * `envValidationSchema` field of the `client` or `server` config in your
 * `main.wasp.ts`.
 *
 * Wasp merges your schema with the built-in validation schema of the
 * Wasp-defined env vars. It validates `import.meta.env` with the client schema
 * and `process.env` with the server schema, and exposes the result as the
 * `env` object from `wasp/client` and `wasp/server`.
 *
 * @param schema A Zod object schema describing your env vars.
 * @returns The same schema.
 *
 * @example
 * ```ts
 * // src/env.ts
 * import * as z from "zod";
 * import { defineEnvValidationSchema } from "wasp/env";
 *
 * export const serverEnvValidationSchema = defineEnvValidationSchema(
 *   z.object({
 *     STRIPE_API_KEY: z.string({ error: "STRIPE_API_KEY is required." }),
 *   }),
 * );
 *
 * export const clientEnvValidationSchema = defineEnvValidationSchema(
 *   z.object({
 *     REACT_APP_ANALYTICS_ID: z.string({
 *       error: "REACT_APP_ANALYTICS_ID is required.",
 *     }),
 *     REACT_APP_NAME: z.string().default("TODO App"),
 *   }),
 * );
 * ```
 *
 * ```ts
 * // main.wasp.ts
 * import { app } from "@wasp.sh/spec";
 * import {
 *   clientEnvValidationSchema,
 *   serverEnvValidationSchema,
 * } from "./src/env" with { type: "ref" };
 *
 * export default app({
 *   name: "myApp",
 *   client: {
 *     envValidationSchema: clientEnvValidationSchema,
 *   },
 *   server: {
 *     envValidationSchema: serverEnvValidationSchema,
 *   },
 *   // ...
 * });
 * ```
 *
 * @see [Custom env var validations](https://wasp.sh/docs/advanced/env-vars#custom-env-var-validations)
 */
export { defineEnvValidationSchema } from "@wasp.sh/lib-sdk-core";
