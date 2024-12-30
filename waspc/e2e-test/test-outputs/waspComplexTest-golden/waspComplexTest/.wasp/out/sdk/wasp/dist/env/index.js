import * as z from 'zod';
const redColor = '\x1b[31m';
// PRIVATE API (SDK, Vite config)
export function ensureEnvSchema(data, schema) {
    const result = getValidatedDataOrError(data, schema);
    switch (result.type) {
        case 'error':
            console.error(`${redColor}${result.message}`);
            throw new Error('Error parsing environment variables');
        case 'success':
            return result.data;
        default:
            result;
    }
}
// PRIVATE API (SDK, Vite config)
export function getValidatedDataOrError(data, schema) {
    try {
        const validatedData = schema.parse(data);
        return {
            type: 'success',
            data: validatedData,
        };
    }
    catch (e) {
        if (e instanceof z.ZodError) {
            const errorOutput = [
                '',
                '|══ Env vars validation failed ══',
                '|',
            ];
            for (const error of e.errors) {
                errorOutput.push(`| - ${error.message}`);
            }
            errorOutput.push('|');
            errorOutput.push('|════════════════════════════════');
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
//# sourceMappingURL=index.js.map