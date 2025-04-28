import { DbType, setupDb } from "../db/index.js";
import { buildApp } from "../waspCli.js";
import { buildClientApp, startClientApp } from "./client.js";
import { buildAndRunServerAppContainer } from "./server.js";

export async function startAppInBuildMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
}: {
  waspCliCmd: string;
  pathToApp: string;
  appName: string;
  dbType: DbType;
}) {
  await buildApp({
    waspCliCmd,
    pathToApp,
  });

  await buildClientApp({
    pathToApp,
  });

  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await buildAndRunServerAppContainer({
    appName,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  startClientApp({
    pathToApp,
  });
}
