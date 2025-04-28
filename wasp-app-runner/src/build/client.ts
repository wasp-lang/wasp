import * as path from "path";
import { spawnWithLog } from "../process.js";

export function buildClientApp({
  pathToApp,
}: {
  pathToApp: string;
}): Promise<{ exitCode: number | null }> {
  return spawnWithLog({
    name: "client-build-app",
    cmd: "npm",
    args: ["run", "build"],
    cwd: path.join(pathToApp, ".wasp/build/web-app"),
    extraEnv: {
      REACT_APP_API_URL: "http://localhost:3001",
    },
  });
}

export async function startClientApp({
  pathToApp,
}: {
  pathToApp: string;
}): Promise<{
  exitCode: number | null;
}> {
  return spawnWithLog({
    name: "client-start-app",
    cmd: "npx",
    args: ["serve", "--single", "-p", "3000"],
    cwd: path.join(pathToApp, ".wasp/build/web-app/build"),
  });
}
