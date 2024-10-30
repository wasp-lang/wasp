import * as z from 'zod'

export const serverEnvValidationFn = () =>
  z.object({
    MY_ENV_VAR: z.string({
      required_error: 'MY_ENV_VAR is required.',
    }),
  })

export const clientEnvValidationFn = () =>
  z.object({
    REACT_APP_NAME: z.string().default('TODO App'),
  })
