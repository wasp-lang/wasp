import * as z from 'zod';
import { getColorizedConsoleFormatString } from 'wasp/universal/ansiColors';
const redColorFormatString = getColorizedConsoleFormatString('red');
// PRIVATE API (SDK)
export function ensureEnvSchema(data, schema) {
    const result = getValidatedEnvOrError(data, schema);
    switch (result.type) {
        case 'error':
            console.error(redColorFormatString, result.message);
            throw new Error('Error parsing environment variables');
        case 'success':
            return result.data;
        default:
            result;
    }
}
// PRIVATE API (SDK, Vite config)
export function getValidatedEnvOrError(env, schema) {
    try {
        const validatedEnv = schema.parse(env);
        return {
            type: 'success',
            data: validatedEnv,
        };
    }
    catch (e) {
        if (e instanceof z.ZodError) {
            const errorOutput = ['', '══ Env vars validation failed ══', ''];
            for (const error of e.errors) {
                errorOutput.push(` - ${error.message}`);
            }
            errorOutput.push('');
            errorOutput.push('════════════════════════════════');
            return {
                type: 'error',
                message: errorOutput.join('\n'),
            };
        }
        else {
            return {
                type: 'error',
                message: e.message,
            };
        }
    }
}
//# sourceMappingURL=validation.js.map