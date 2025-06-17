export const serverUrlEnvVarName = "REACT_APP_API_URL";

export function getServerUrlFromEnv(defaultValue: string): string {
  return process.env[serverUrlEnvVarName]
    ? process.env[serverUrlEnvVarName]
    : defaultValue;
}
