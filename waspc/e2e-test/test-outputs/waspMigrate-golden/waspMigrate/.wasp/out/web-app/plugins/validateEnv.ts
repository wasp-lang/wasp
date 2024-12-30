import { type Plugin, loadEnv } from "vite";

import { getValidatedDataOrError, type SchemaParsingResult } from 'wasp/env'
import { clientEnvSchema } from 'wasp/client/env/schema'

const RED_COLOR = '\x1b[31m'

export function validateEnv(): Plugin {
  let _validationResult: SchemaParsingResult = null
  return {
    name: 'wasp-validate-env',
    configResolved: (config) => {
      const env = loadEnv(config.mode, process.cwd(), config.envPrefix)

      _validationResult = getValidatedDataOrError(env, clientEnvSchema)

      if (_validationResult.type !== 'error') {
        return;
      }

      if (config.command !== 'build') {
        return;
      }
      
      console.error(`${RED_COLOR}${_validationResult.message}`)
      // Exit early if we are in build mode, because we can't show the error in the browser.
      process.exit(1)
    },
    configureServer: (server) => {
      if (_validationResult.type !== 'error') {
        return;
      }

      // Send the error to the browser.
      server.ws.on('connection', (ws) => {
        server.ws.send({
          type: 'error',
          err: {
            message: _validationResult.message,
            stack: ""
          }
        })
      })
    }
  };
}
