import * as z from 'zod'

const redColor = '\x1b[31m'

export function ensureEnvSchema<Schema extends z.ZodTypeAny>(
  data: unknown,
  schema: Schema
): z.infer<Schema> {
  try {
    return schema.parse(data)
  } catch (e) {
    // TODO: figure out how to output the error message in a better way
    if (e instanceof z.ZodError) {
      console.error()
      console.error(redColor, '╔═════════════════════════════╗');
      console.error(redColor, '║ Env vars validation failed  ║');
      console.error(redColor, '╚═════════════════════════════╝');
      console.error()
      for (const error of e.errors) {
        console.error(redColor, `- ${error.message}`)
      }
      console.error()
      console.error(redColor, '═══════════════════════════════');
      throw new Error('Error parsing environment variables')
    } else {
      throw e
    }
  }
}
