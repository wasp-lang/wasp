import { WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import {
  getClientBuildArtefactsDir,
  getClientBuildDir,
} from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";

export const serverUrlEnvVarName = "REACT_APP_API_URL";

export async function buildClient(
  defaultServerAppUrl: string,
  { waspProjectDir }: { waspProjectDir: WaspProjectDir },
): Promise<string> {
  waspSays("Building web client for production...");

  const clientBuildDir = getClientBuildDir(waspProjectDir);
  const npmCli = createCommandWithCwd("npm", clientBuildDir);
  const npxCli = createCommandWithCwd("npx", clientBuildDir);

  await npmCli(["install"]);

  const serverUrl = getServerAppUrlEnvValue() ?? defaultServerAppUrl;
  await npxCli(["vite", "build"], {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  });

  return getClientBuildArtefactsDir(waspProjectDir);
}

// We allow users to specify the server URL that the client will connect to
// using an environment variable.
// TODO: improve this API to allow specifying the server URL via options or a config file.
function getServerAppUrlEnvValue(): string | undefined {
  return process.env[serverUrlEnvVarName];
}
