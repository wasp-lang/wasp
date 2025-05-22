import { chalk } from 'zx'
import {
  applyPatch,
  migrateDb,
  writeFileToAppDir,
  type Action,
} from '../actions'
import { log } from '../log'
import { ensureDirExists, patchesDir } from '../paths'

export async function executeSteps(
  actions: Action[],
  {
    untilStep,
  }: {
    untilStep?: number
  }
): Promise<void> {
  for (const action of actions) {
    if (untilStep && action.step === untilStep) {
      log('info', `Stopping before step ${action.step}`)
      process.exit(0)
    }

    const kind = action.kind
    log('info', `${chalk.bold(`[step ${action.step}]`)} ${kind}`)

    // Prepare the patches directory
    await ensureDirExists(patchesDir)

    try {
      switch (kind) {
        case 'diff':
          await applyPatch(action)
          break
        case 'write':
          await writeFileToAppDir(action)
          break
        case 'migrate-db':
          await migrateDb(`step-${action.step}`)
          break
        default:
          kind satisfies never
      }
    } catch (err) {
      log('error', `Error in step ${action.step}:\n\n${err}`)
      process.exit(1)
    }
  }
}
