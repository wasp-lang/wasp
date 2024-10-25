import * as z from 'zod'

export function ensureEnvSchema<Schema extends z.ZodTypeAny>(
	data: unknown,
	schema: Schema,
): z.infer<Schema> {
	try {
		return schema.parse(data)
	} catch (e) {
    // TODO: figure out how to output the error message in a better way
		if (e instanceof z.ZodError) {
			throw new Error(e.errors.map((error) => error.message).join('\n'))
		} else {
			throw e
		}
	}
}
