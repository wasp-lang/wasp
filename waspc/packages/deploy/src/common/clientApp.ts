import { WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import { getClientBuildDir } from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";

export const serverUrlEnvVarName = "REACT_APP_API_URL";

export async function buildClient(
  defaultServerAppUrl: string,
  { waspProjectDir }: { waspProjectDir: WaspProjectDir },
): Promise<void> {
  waspSays("Building web client for production...");
  waspSays(
    `If you configured a custom domain for the server, you should run the command with an env variable: ${serverUrlEnvVarName}=https://serverUrl.com <command>`,
  );

  const clientBuildDir = getClientBuildDir(waspProjectDir);
  const npmCli = createCommandWithCwd("npm", clientBuildDir);

  await npmCli(["install"]);

  const serverUrl = getServerAppUrlEnvValue() ?? defaultServerAppUrl;
  await npmCli(["run", "build"], {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  });
}

// We allow users to specify the server URL that the client will connect to
// using an environment variable.
// TODO: improve this API to allow specifying the server URL via options or a config file.
function getServerAppUrlEnvValue(): string | undefined {
  return process.env[serverUrlEnvVarName];
}
