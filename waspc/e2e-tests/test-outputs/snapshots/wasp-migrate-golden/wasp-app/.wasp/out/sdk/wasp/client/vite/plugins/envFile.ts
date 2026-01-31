import { type Plugin, type UserConfig } from 'vite'
import { resolve } from 'node:path'
import { existsSync, readFileSync } from 'node:fs'
import { parse as parseDotenv } from 'dotenv'
import { expand, type DotenvPopulateInput } from 'dotenv-expand'

const envFileName = '.env.client'

// Based on: https://github.com/vitejs/vite/blob/8bb32036792a6f522f5c947112f3d688add755a0/packages/vite/src/node/env.ts
export function loadWaspEnvClient(rootDir: string, envPrefix: NonNullable<UserConfig['envPrefix']>): Record<string, string> {
  const envPrefixNormalized = Array.isArray(envPrefix) ? envPrefix : [envPrefix]
  const envFilePath = resolve(rootDir, envFileName)

  if (!existsSync(envFilePath)) {
    return {}
  }

  try {
    const parsed = parseDotenv(readFileSync(envFilePath, 'utf-8'))

    // Let environment variables use each other. Make a copy of `process.env` so that `dotenv-expand`
    // doesn't re-assign the expanded values to the global `process.env`.
    const processEnv = { ...process.env } as DotenvPopulateInput
    expand({ parsed, processEnv })

    const env: Record<string, string> = {}

    // Only keys that start with prefix are exposed to client.
    for (const [key, value] of Object.entries(parsed || {})) {
      if (envPrefixNormalized.some(prefix => key.startsWith(prefix))) {
        env[key] = value
      }
    }

    // Check if there are actual env variables starting with the prefix
    // these are typically provided inline and should be prioritized.
    for (const key in process.env) {
      if (envPrefixNormalized.some(prefix => key.startsWith(prefix))) {
        env[key] = process.env[key] as string
      }
    }

    return env
  } catch (error) {
    console.error(`Error parsing ${envFileName}:`, error)
    throw error
  }
}


export function envFile(): Plugin {
  return {
    name: 'wasp:env-file',
    enforce: 'pre',
    config(config) {
      const envVars = loadWaspEnvClient(config.root || process.cwd(), config.envPrefix!)

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
  }
}
