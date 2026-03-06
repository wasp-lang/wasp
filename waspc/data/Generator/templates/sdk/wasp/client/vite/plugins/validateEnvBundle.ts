{{={= =}=}}
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'
import { type Plugin } from 'vite'
import {
  getValidatedEnvOrError,
  formatZodEnvErrors,
} from '../../../env/validation.js'
import { loadEnvVars } from './envFile.js'
import { getColorizedConsoleFormatString } from '../../../universal/ansiColors.js'

const redColorFormatString = getColorizedConsoleFormatString('red');

// Hackish
export function validateEnvBundle(): Plugin {
  let validationResult: any;
  return {
    name: 'wasp:validate-env-bundle',
    async configResolved(config) {
      const __dirname = path.dirname(fileURLToPath(import.meta.url))
      const schemaPath = path.resolve(__dirname, '../../env/schema.js')

      {=# userEnvSchemaPath =}
      const userEnvSchemaPath = path.resolve(config.root, '{=& userEnvSchemaPath =}')
      {=/ userEnvSchemaPath =}

      const env = await loadEnvVars({
        rootDir: config.root,
        envPrefix: config.envPrefix!,
        loadDotEnvFile: config.command === 'serve',
      })

      const entryCode = [
        `import { clientEnvSchema } from ${JSON.stringify(schemaPath)}`,
        `export const schema = clientEnvSchema`,
      ].join('\n')

      const esbuild = await import('esbuild')
      const buildResult = await esbuild.build({
        stdin: {
          contents: entryCode,
          resolveDir: config.root,
          loader: 'js',
        },
        bundle: true,
        write: false,
        format: 'esm',
        platform: 'node',
        logLevel: 'silent',
        {=# userEnvSchemaPath =}
        plugins: [
          {
            name: 'resolve-virtual-modules',
            setup(build) {
              build.onResolve(
                { filter: /^virtual:wasp\// },
                (args) => build.resolve(userEnvSchemaPath, {
                  kind: args.kind,
                  resolveDir: config.root,
                })
              )
            },
          },
        ],
        {=/ userEnvSchemaPath =}
      })

      const code = buildResult.outputFiles[0].text
      const dataUrl = `data:text/javascript;base64,${Buffer.from(code).toString('base64')}`
      const mod = await import(dataUrl)
      validationResult = getValidatedEnvOrError(env, mod.schema)

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
