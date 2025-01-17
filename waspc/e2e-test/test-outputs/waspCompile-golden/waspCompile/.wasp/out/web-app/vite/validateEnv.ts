import { type Plugin, loadEnv } from 'vite'

import {
  getValidatedEnvOrError,
  type EnvValidationResult,
} from 'wasp/env/validation'
import { clientEnvSchema } from 'wasp/client/env/schema'

const redColor = '\x1b[31m'

export function validateEnv(): Plugin {
  let validationResult: EnvValidationResult<unknown> | null = null
  return {
    name: 'wasp-validate-env',
    configResolved: (config) => {
      const env = loadEnv(config.mode, process.cwd(), config.envPrefix)

      validationResult = getValidatedEnvOrError(env, clientEnvSchema)

      if (validationResult.type !== 'error') {
        return
      }

      if (config.command !== 'build') {
        return
      }

      console.error(`${redColor}${validationResult.message}`)
      // Exit early if we are in build mode, because we can't show the error in the browser.
      process.exit(1)
    },
    configureServer: (server) => {
      if (validationResult === null || validationResult.type !== 'error') {
        return
      }

      // Send the error to the browser.
      const message = validationResult.message
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
