{{={= =}=}}
import * as z from 'zod'

// PRIVATE API (SDK, Vite config)
export const clientEnvSchema = z.object({
  REACT_APP_API_URL: z
    .string()
    .url({
      message: 'REACT_APP_API_URL must be a valid URL',
    })
    .default('{= defaultServerUrl =}'),
})
