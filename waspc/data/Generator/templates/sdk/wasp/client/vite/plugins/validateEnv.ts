import { type Plugin } from 'vite'

import {
  formatZodEnvError,
  getValidatedEnvOrError,
} from '../../../env/validation.js'
import { getColorizedConsoleFormatString } from '../../../universal/ansiColors.js'
import { getClientEnvSchema } from '../../env/schema.js'
import { loadEnvVars } from './envFile.js'

const redColorFormatString = getColorizedConsoleFormatString('red');

export function validateEnv(): Plugin {
  let validationResult: ReturnType<typeof getValidatedEnvOrError> | null = null
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
      const schema = getClientEnvSchema(config.mode)
      validationResult = getValidatedEnvOrError(env, schema)

      // Exit if we are in build mode, because we can't show the error in the browser.
      if (config.command === 'build' && !validationResult.success) {
        const message = formatZodEnvError(validationResult.error)
        console.error(`${redColorFormatString}${message}`)
        process.exit(1)
      }
    },
    configureServer: (server) => {
      if (validationResult === null || validationResult.success) {
        return
      }

      // Send the error to the browser.
      const message = formatZodEnvError(validationResult.error)
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
