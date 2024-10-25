import * as z from 'zod'

import { ensureEnvSchema } from '../../env/index.js'

const serverEnvSchema = z.object({
	NODE_ENV: z.enum(['development', 'production']).default('development'),
	PORT: z.coerce.number().default(3000),
	SERVER_URL: z.string({
		required_error: 'SERVER_URL is required',
	}),
	CLIENT_URL: z.string({
		required_error: 'CLIENT_URL is required',
	}),
	JWT_SECRET: z.string({
		required_error: 'JWT_SECRET is required',
	}),
})

export const env = ensureEnvSchema(process.env, serverEnvSchema)
