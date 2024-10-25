import * as z from 'zod'

import { ensureEnvSchema } from '../../env/index.js'

const serverEnvSchema = z.object({
	REACT_APP_API_URL: z.string({
		required_error: 'REACT_APP_API_URL is required',
	}),
})

export const env = ensureEnvSchema(import.meta.env, serverEnvSchema)
