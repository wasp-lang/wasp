import { type ProviderConfig } from "wasp/auth/providers/types";

export function ensureEnvVarsForProvider<EnvVarName extends string>(
  envVarNames: EnvVarName[],
  provider: ProviderConfig,
): Record<EnvVarName, string> {
  const result: Record<string, string> = {};
  for (const envVarName of envVarNames) {
    const value = process.env[envVarName];
    if (!value) {
      throw new Error(`${envVarName} env variable is required when using the ${provider.displayName} auth provider.`);
    }
    result[envVarName] = value;
  }
  return result as Record<EnvVarName, string>;
}
