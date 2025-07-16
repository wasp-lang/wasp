import { $ } from "zx";

export async function waspDbMigrate(
  appDir: string,
  migrationName: string,
): Promise<void> {
  await $({
    // Needs to inhert stdio for `wasp db migrate-dev` to work
    stdio: "inherit",
  })`cd ${appDir} && wasp db migrate-dev --name ${migrationName}`;
}

export async function waspNew(appDir: string): Promise<void> {
  await $`wasp new ${appDir}`;
}
