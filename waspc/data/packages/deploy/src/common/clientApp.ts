import { WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import {
  getClientBuildArtefactsDir,
  getClientBuildDir,
} from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";

const serverUrlEnvVarName = "REACT_APP_API_URL";

export async function buildClient(
  serverUrl: string,
  { waspProjectDir }: { waspProjectDir: WaspProjectDir },
): Promise<string> {
  waspSays("Building web client for production...");

  const clientBuildDir = getClientBuildDir(waspProjectDir);
  const npmCli = createCommandWithCwd("npm", clientBuildDir);

  await npmCli(["install"]);

  await npmCli(["run", "build"], {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  });

  return getClientBuildArtefactsDir(waspProjectDir);
}
