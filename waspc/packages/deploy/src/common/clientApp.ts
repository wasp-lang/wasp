export const serverUrlEnvVarName = "REACT_APP_API_URL";

// We allow users to specify the server URL that the client will connect to
// using an environment variable.
// TODO: improve this API to allow specifying the server URL via options or a config file.
export function getServerAppUrlFromEnv(defaultValue: string): string {
  return process.env[serverUrlEnvVarName]
    ? process.env[serverUrlEnvVarName]
    : defaultValue;
}
