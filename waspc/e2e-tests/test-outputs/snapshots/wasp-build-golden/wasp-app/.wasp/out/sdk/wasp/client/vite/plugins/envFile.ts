import { type Plugin, type UserConfig } from 'vite'
import { resolve } from 'node:path'
import { readFile, access, constants } from 'node:fs/promises'
import { parse as parseDotenv } from 'dotenv'
import { expand, type DotenvPopulateInput } from 'dotenv-expand'

const envFileName = '.env.client'

/**
 * The env file Wasp generates for the server. Unlike the client's, this one is
 * generated (from the user's `.env.server`) and only exists in development.
 */
const serverEnvFileName = '.wasp/out/server/.env'

export function envFile(): Plugin {
  let envFilePath!: string
  let serverEnvFilePath!: string
  return {
    name: 'wasp:env-file',
    enforce: 'pre',
    async config(config, env) {
      const rootDir = config.root || process.cwd()
      serverEnvFilePath = resolve(rootDir, serverEnvFileName)

      if (env.command === 'serve') {
        await loadServerEnvVarsIntoProcessEnv(serverEnvFilePath)
      }

      const envVars = await loadEnvVars({
        rootDir,
        // We are sure that `envPrefix` is defined because
        // we defined it in an earlier plugin.
        envPrefix: config.envPrefix!,
        // We load the env file variables only in development,
        // when building for production, users are expected to
        // provide the environment variables inline.
        loadDotEnvFile: env.command === 'serve',
      })
      envFilePath = resolve(rootDir, envFileName)

      const prefixedVars = Object.entries(envVars)
        .reduce((acc, [key, value]) => {
          acc[`import.meta.env.${key}`] = JSON.stringify(value)
          return acc
        }, {} as Record<string, string>)

      return {
        // Disable Vite's default .env loading.
        //
        // Note that this doesn't stop Nitro, which loads the project's `.env`
        // files into `process.env` on its own. It can't affect the variables
        // we expose to the client though: this plugin reads `process.env`
        // in its `config` hook, which runs before Nitro's.
        envDir: false,
        define: prefixedVars,
      }
    },
    configureServer(server) {
      const reloadServerOnEnvFileEvent = (path: string) => {
        if (path === envFilePath || path === serverEnvFilePath) {
          server.restart()
        }
      }

      server.watcher.on('add', reloadServerOnEnvFileEvent)
      server.watcher.on('change', reloadServerOnEnvFileEvent)
      server.watcher.on('unlink', reloadServerOnEnvFileEvent)
    },
    async buildStart() {
      this.addWatchFile(envFilePath)
    },
  }
}

/**
 * Your server's code runs in this same process now (it is part of the app Vite
 * serves), so its environment variables have to be in `process.env` before it
 * starts.
 *
 * We only do this in development: the env file only exists then, and in
 * production the environment is the deployment's business.
 *
 * These variables never reach the browser: they only go into `process.env`,
 * never into the `import.meta.env` values we define for the client.
 */
async function loadServerEnvVarsIntoProcessEnv(
  serverEnvFilePath: string
): Promise<void> {
  const envVars = await parseEnvFile(serverEnvFilePath)
  for (const [key, value] of Object.entries(envVars)) {
    // Same rule `dotenv` follows: a variable that is already defined (in the
    // shell, for example) wins over the file.
    if (process.env[key] === undefined) {
      process.env[key] = value
    }
  }
}

// Based on: https://github.com/vitejs/vite/blob/8bb32036792a6f522f5c947112f3d688add755a0/packages/vite/src/node/env.ts
export async function loadEnvVars({
  rootDir,
  envPrefix,
  loadDotEnvFile,
}: {
  rootDir: string
  envPrefix: NonNullable<UserConfig['envPrefix']>
  loadDotEnvFile: boolean
}): Promise<Record<string, string>> {
  const envPrefixNormalized = Array.isArray(envPrefix) ? envPrefix : [envPrefix]
  const env: Record<string, string> = {}

  if (loadDotEnvFile) {
    const envFilePath = resolve(rootDir, envFileName)
    const parsed = await parseEnvFile(envFilePath)

    // Let environment variables use each other. Make a copy of `process.env` so that `dotenv-expand`
    // doesn't re-assign the expanded values to the global `process.env`.
    const processEnv = { ...process.env } as DotenvPopulateInput
    expand({ parsed, processEnv })

    // Only keys that start with prefix are exposed to client.
    for (const [key, value] of Object.entries(parsed)) {
      if (envPrefixNormalized.some(prefix => key.startsWith(prefix))) {
        env[key] = value
      }
    }
  }

  // Make sure that inline env variables are prioritized over env file variables.
  // Follows the logic Vite uses for env variables.
  for (const key in process.env) {
    if (envPrefixNormalized.some(prefix => key.startsWith(prefix))) {
      env[key] = process.env[key] as string
    }
  }

  return env
}

async function parseEnvFile(envFilePath: string): Promise<Record<string, string>> {
  try {
    await access(envFilePath, constants.R_OK)
  } catch {
    return {}
  }

  try {
    return parseDotenv(await readFile(envFilePath, 'utf-8'))
  } catch (error) {
    console.error(`Error parsing ${envFilePath}:`, error)
    throw error
  }
}
