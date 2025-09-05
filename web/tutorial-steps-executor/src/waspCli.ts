import { $ } from "zx";
import type { AppDirPath, AppName, AppParentDirPath } from "./brandedTypes";

export async function waspDbMigrate(
  appDir: AppDirPath,
  migrationName: string,
): Promise<void> {
  await $({
    // Needs to inhert stdio for `wasp db migrate-dev` to work
    stdio: "inherit",
    cwd: appDir,
  })`wasp db migrate-dev --name ${migrationName}`;
}

export async function waspNew({
  appName,
  appParentDir,
}: {
  appName: AppName;
  appParentDir: AppParentDirPath;
}): Promise<void> {
  await $({
    cwd: appParentDir,
  })`wasp new ${appName} -t minimal`;
}
