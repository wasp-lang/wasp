import { getColorizedConsoleFormatString } from 'wasp/universal/ansiColors';
const redColorFormatString = getColorizedConsoleFormatString('red');
// PRIVATE API (SDK)
export function ensureEnvSchema(data, schema) {
    const result = getValidatedEnvOrError(data, schema);
    if (result.success) {
        return result.data;
    }
    else {
        console.error(`${redColorFormatString}${formatZodEnvErrors(result.error.issues)}`);
        throw new Error('Error parsing environment variables');
    }
}
// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError(env, schema) {
    return schema.safeParse(env);
}
// PRIVATE API (SDK, Vite config)
export function formatZodEnvErrors(issues) {
    const errorOutput = ['', '══ Env vars validation failed ══', ''];
    for (const error of issues) {
        errorOutput.push(` - ${error.message}`);
    }
    errorOutput.push('');
    errorOutput.push('════════════════════════════════');
    return errorOutput.join('\n');
}
//# sourceMappingURL=validation.js.map