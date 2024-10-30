import * as z from 'zod'
import type { EnvValidationFn } from 'wasp/env'

export const serverEnvValidationFn: EnvValidationFn = () =>
  z.object({
    MY_ENV_VAR: z.string({
      required_error: 'MY_ENV_VAR is required.',
    }),
  })

export const clientEnvValidationFn: EnvValidationFn = () =>
  z.object({
    REACT_APP_NAME: z.string().default('TODO App'),
  })
