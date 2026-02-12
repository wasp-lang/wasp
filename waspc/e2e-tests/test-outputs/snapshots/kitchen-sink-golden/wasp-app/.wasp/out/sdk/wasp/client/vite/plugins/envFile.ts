import { type Plugin, type UserConfig } from 'vite'
import { resolve } from 'node:path'
import { readFile, access, constants } from 'node:fs/promises'
import { parse as parseDotenv } from 'dotenv'
import { expand, type DotenvPopulateInput } from 'dotenv-expand'

const envFileName = '.env.client'

export function envFile(): Plugin {
  let envFilePath!: string
  return {
    name: 'wasp:env-file',
    enforce: 'pre',
    async config(config) {
      const rootDir = config.root || process.cwd()
      const envVars = await loadWaspEnvClient(rootDir, config.envPrefix!)
      envFilePath = resolve(rootDir, envFileName)

      const prefixedVars = Object.entries(envVars)
        .reduce((acc, [key, value]) => {
          acc[`import.meta.env.${key}`] = JSON.stringify(value)
          return acc
        }, {} as Record<string, string>)

      return {
        // Disable Vite's default .env loading.
        envDir: false,
        define: prefixedVars,
      }
    },
    configureServer(server) {
      const reloadServerOnEnvFileEvent = (path: string) => {
        if (path === envFilePath) {
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

// Based on: https://github.com/vitejs/vite/blob/8bb32036792a6f522f5c947112f3d688add755a0/packages/vite/src/node/env.ts
export async function loadWaspEnvClient(rootDir: string, envPrefix: NonNullable<UserConfig['envPrefix']>): Promise<Record<string, string>> {
  const envPrefixNormalized = Array.isArray(envPrefix) ? envPrefix : [envPrefix]
  const envFilePath = resolve(rootDir, envFileName)

  const parsed = await parseEnvFile(envFilePath)

  // Let environment variables use each other. Make a copy of `process.env` so that `dotenv-expand`
  // doesn't re-assign the expanded values to the global `process.env`.
  const processEnv = { ...process.env } as DotenvPopulateInput
  expand({ parsed, processEnv })

  const env: Record<string, string> = {}

  // Only keys that start with prefix are exposed to client.
  for (const [key, value] of Object.entries(parsed)) {
    if (envPrefixNormalized.some(prefix => key.startsWith(prefix))) {
      env[key] = value
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
    console.error(`Error parsing ${envFileName}:`, error)
    throw error
  }
}
