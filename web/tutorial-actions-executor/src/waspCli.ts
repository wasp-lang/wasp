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
    // Needs to inherit stdio for `wasp db migrate-dev` to work
    stdio: "inherit",
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
