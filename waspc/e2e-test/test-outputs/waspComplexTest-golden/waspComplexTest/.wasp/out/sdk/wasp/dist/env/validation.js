import * as z from 'zod';
const redColor = '\x1b[31m';
export function ensureEnvSchema(data, schema) {
    try {
        return schema.parse(data);
    }
    catch (e) {
        if (e instanceof z.ZodError) {
            console.error();
            console.error(redColor, '╔═════════════════════════════╗');
            console.error(redColor, '║ Env vars validation failed  ║');
            console.error(redColor, '╚═════════════════════════════╝');
            console.error();
            for (const error of e.errors) {
                console.error(redColor, `- ${error.message}`);
            }
            console.error();
            console.error(redColor, '═══════════════════════════════');
            throw new Error('Error parsing environment variables');
        }
        else {
            throw e;
        }
    }
}
//# sourceMappingURL=validation.js.map