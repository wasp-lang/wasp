import { type Plugin } from 'vite'

import { loadEnvVars } from './envFile.js'
import {
  getValidatedZodEnvOrError,
  getValidatedStandardSchemaEnvOrError,
  formatZodEnvErrors,
  formatStandardSchemaErrors,
} from '../../../env/validation.js'
import { getClientWaspEnvSchema, userClientEnvSchema } from '../../env/schema.js'
import { getColorizedConsoleFormatString } from '../../../universal/ansiColors.js'

const redColorFormatString = getColorizedConsoleFormatString('red');

export function validateEnv(): Plugin {
  let errorMessage: string | null = null
  return {
    name: 'wasp:validate-env',
    async configResolved(config) {
      const env = await loadEnvVars({
        rootDir: config.root,
        // We are sure that `envPrefix` is defined because
        // we defined it in an earlier plugin.
        envPrefix: config.envPrefix!,
        // We load the env file variables only in development,
        // when building for production, users are expected to
        // provide the environment variables inline.
        loadDotEnvFile: config.command === 'serve',
      })

      const errors: string[] = []

      const waspSchema = getClientWaspEnvSchema(config.mode)
      const waspResult = getValidatedZodEnvOrError(env, waspSchema)
      if (!waspResult.success) {
        errors.push(formatZodEnvErrors(waspResult.error.issues))
      }

      if (userClientEnvSchema) {
        const userResult = getValidatedStandardSchemaEnvOrError(env, userClientEnvSchema)
        if (userResult.issues) {
          errors.push(formatStandardSchemaErrors(userResult.issues))
        }
      }

      if (errors.length > 0) {
        errorMessage = errors.join('\n')

        // Exit if we are in build mode, because we can't show the error in the browser.
        if (config.command === 'build') {
          console.error(`${redColorFormatString}${errorMessage}`)
          process.exit(1)
        }
      }
    },
    configureServer: (server) => {
      if (!errorMessage) {
        return
      }

      // Send the error to the browser.
      server.ws.on('connection', () => {
        server.ws.send({
          type: 'error',
          err: {
            message: errorMessage!,
            stack: '',
          },
        })
      })
    },
  }
}
