import { type Plugin, loadEnv } from 'vite'

import {
  getValidatedEnvOrError,
  formatZodEnvErrors,
} from 'wasp/env/validation'
import { clientEnvSchema } from 'wasp/client/env/schema'
import { getColorizedConsoleFormatString } from 'wasp/universal/ansiColors'

const redColorFormatString = getColorizedConsoleFormatString('red');

export function validateEnv(): Plugin {
  let validationResult: ReturnType<typeof getValidatedEnvOrError> | null = null
  return {
    name: 'wasp-validate-env',
    configResolved: (config) => {
      const env = loadEnv(config.mode, process.cwd(), config.envPrefix)
      validationResult = getValidatedEnvOrError(env, clientEnvSchema)

      // Exit if we are in build mode, because we can't show the error in the browser.
      if (config.command === 'build' && !validationResult.success) {
        const message = formatZodEnvErrors(validationResult.error.issues)
        console.error(`${redColorFormatString}${message}`)
        process.exit(1)
      }
    },
    configureServer: (server) => {
      if (validationResult === null || validationResult.success) {
        return
      }

      // Send the error to the browser.
      const message = formatZodEnvErrors(validationResult.error.issues)
      server.ws.on('connection', () => {
        server.ws.send({
          type: 'error',
          err: {
            message,
            stack: '',
          },
        })
      })
    },
  }
}
