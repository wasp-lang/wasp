import * as z from "zod";
import { colorize } from "../universal/ansiColors.js";
// PRIVATE API (SDK)
export function ensureEnvSchema(data, schema) {
    const result = getValidatedEnvOrError(data, schema);
    if (result.success) {
        return result.data;
    }
    else {
        console.error(colorize("red", formatZodEnvError(result.error)));
        throw new Error("Error parsing environment variables");
    }
}
// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError(env, schema) {
    return schema.safeParse(env);
}
// PRIVATE API (SDK, Vite config)
export function formatZodEnvError(error) {
    const flattenedIssues = z.flattenError(error);
    return [
        "══ Env vars validation failed ══",
        "",
        // Top-level errors
        ...flattenedIssues.formErrors,
        "",
        // Errors per field
        ...Object.entries(flattenedIssues.fieldErrors).map(([prop, error]) => `${prop} - ${error}`),
        "",
        "════════════════════════════════",
    ].join("\n");
}
//# sourceMappingURL=validation.js.map