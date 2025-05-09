import path from 'path'

import { $, chalk } from 'zx'

import { applyPatch, writeFileToAppDir, type Action } from './actions'
import { appDir } from './paths'
import { waspDbMigrate, waspNew } from './waspCli'
import { log } from './log'
import { getActionsFromTutorialFiles } from './markdown/extractSteps'
import { program } from '@commander-js/extra-typings'

const options = program
  .option(
    '-s, --until-step <step>',
    'Run until the given step. If not provided, run all steps.',
    (value: string) => {
      const step = parseInt(value, 10)
      if (isNaN(step) || step < 1) {
        throw new Error('Step must be a positive integer.')
      }
      return step
    }
  )
  .option(
    '-e, --edit',
    'Edit mode, you will be offered to edit the diffs',
    false
  )
  .parse(process.argv)
  .opts()

$.verbose = true

const actions: Action[] = await getActionsFromTutorialFiles()

async function prepareApp() {
  await $`rm -rf ${appDir}`
  await waspNew(appDir)
  await $`rm ${path.join(appDir, 'src/Main.css')}`
  await $`rm ${path.join(appDir, 'src/waspLogo.png')}`
  await $`rm ${path.join(appDir, 'src/MainPage.jsx')}`
  // Git needs to be initialized for patches to work
  await $`cd ${appDir} && git init`
}

await prepareApp()

for (const action of actions) {
  if (options.untilStep && action.step === options.untilStep) {
    log('info', `Stopping before step ${action.step}`)
    process.exit(0)
  }

  const kind = action.kind
  log('info', `${chalk.bold(`Step ${action.step}`)}: ${kind}`)

  try {
    switch (kind) {
      case 'diff':
        if (options.edit) {
          // Ask the user if they want to change the diff
          // If yes, don't apply the diff, let them edit manually and generate a new diff
          // Display the diff to the user
        } else {
          await applyPatch(action)
        }
        break
      case 'write':
        await writeFileToAppDir(action)
        break
      case 'migrate-db':
        await waspDbMigrate(`step-${action.step}`)
        break
      default:
        kind satisfies never
    }
  } catch (err) {
    log('error', `Error in step ${action.step}: ${err}`)
    process.exit(1)
  }
}

log('info', 'All done!')
