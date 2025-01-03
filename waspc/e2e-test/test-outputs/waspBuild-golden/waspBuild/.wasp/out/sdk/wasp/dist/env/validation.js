import * as z from 'zod';
const redColor = '\x1b[31m';
export function ensureEnvSchema(data, schema) {
    try {
        return schema.parse(data);
    }
    catch (e) {
        if (e instanceof z.ZodError) {
            const errorOutput = ['', '══ Env vars validation failed ══', ''];
            for (const error of e.errors) {
                errorOutput.push(` - ${error.message}`);
            }
            errorOutput.push('');
            errorOutput.push('════════════════════════════════');
            console.error(redColor, errorOutput.join('\n'));
            throw new Error('Error parsing environment variables');
        }
        else {
            throw e;
        }
    }
}
//# sourceMappingURL=validation.js.map