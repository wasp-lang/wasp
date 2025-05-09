import { $ } from 'zx'

import { appDir } from './paths'

export async function waspDbMigrate(migrationName: string): Promise<void> {
  await $({
    // Needs to inhert stdio for `wasp db migrate-dev` to work
    stdio: 'inherit',
  })`cd ${appDir} && wasp db migrate-dev --name ${migrationName}`
}

export async function waspNew(appName: string): Promise<void> {
  await $`wasp new ${appDir}`
}
