{{={= =}=}}
import { envFile as coreEnvFile, loadEnvVars as coreLoadEnvVars } from '@wasp.sh/lib-sdk-core/node/vite'

const envFileName = '{= clientEnvFileName =}'

export function envFile() {
  return coreEnvFile(envFileName)
}

export function loadEnvVars(options: Omit<Parameters<typeof coreLoadEnvVars>[0], 'envFileName'>) {
  return coreLoadEnvVars({ ...options, envFileName })
}
