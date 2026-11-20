import path from "node:path";
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
  // The project lists Vite itself, so every package manager puts its
  // executable here.
  const viteCli = createCommandWithCwd(
    path.join(clientBuildDir, "node_modules", ".bin", "vite"),
    clientBuildDir,
  );

  await viteCli(["build"], {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  });

  return getClientBuildArtefactsDir(waspProjectDir);
}
