import * as z from 'zod';
const redColor = '\x1b[31m';
export function ensureEnvSchema(data, schema) {
    try {
        return schema.parse(data);
    }
    catch (e) {
        // TODO: figure out how to output the error message in a better way
        if (e instanceof z.ZodError) {
            console.error(redColor, '╔═════════════════════════════╗');
            console.error(redColor, '║ Env vars validation failed  ║');
            console.error(redColor, '╚═════════════════════════════╝');
            console.error();
            for (const error of e.errors) {
                console.error(`- ${error.message}`);
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
//# sourceMappingURL=index.js.map