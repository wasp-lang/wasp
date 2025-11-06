import { $ } from "zx";

import type { Branded } from "./brandedTypes";
import type { AppDirPath, AppName, AppParentDirPath } from "./tutorialApp";

export type WaspCliCommand = Branded<string, "WaspCliCommand">;

export async function waspDbMigrate({
  waspCliCommand,
  appDir,
  migrationName,
}: {
  waspCliCommand: WaspCliCommand;
  appDir: AppDirPath;
  migrationName: string;
}): Promise<void> {
  await $({
    // We ignore stdin to avoid hangs in non-interactive environments e.g. e2e tests when
    // `wasp db migrate-dev` runs. When `stdin` is not ignored, the command waits forever
    // for user input. We don't need any input since we are providing the migration name
    // via the --name flag.
    stdio: ["ignore", "pipe", "pipe"],
    cwd: appDir,
  })`${waspCliCommand} db migrate-dev --name ${migrationName}`;
}

export async function waspNew({
  waspCliCommand,
  appName,
  appParentDirPath,
}: {
  waspCliCommand: WaspCliCommand;
  appName: AppName;
  appParentDirPath: AppParentDirPath;
}): Promise<void> {
  await $({
    cwd: appParentDirPath,
  })`${waspCliCommand} new ${appName} -t minimal`;
}
