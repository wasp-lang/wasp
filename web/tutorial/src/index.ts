import path from 'path'

import { $, chalk } from 'zx'

import fs from 'fs/promises'

import {
  applyPatch,
  migrateDb,
  writeFileToAppDir,
  type Action,
} from './actions/index'
import { appDir, ensureDirExists, patchesDir } from './paths'
import { waspNew } from './waspCli'
import { log } from './log'
import { getActionsFromTutorialFiles } from './markdown/extractSteps'
import { program } from '@commander-js/extra-typings'

import Enquirer from 'enquirer'
import { generateGitPatch, makeCheckpoint } from './edit/generate-patch'
import { executeSteps } from './execute-steps'
import { write } from 'fs'

const actions: Action[] = await getActionsFromTutorialFiles()

const { editDiff, untilStep } = program
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
    '-e, --edit-diff <step>',
    'Edit mode, you will edit the diff interactively and all the steps related to the same file that come after.',
    (value: string) => {
      const step = parseInt(value, 10)
      if (isNaN(step) || step < 1) {
        throw new Error('Step must be a positive integer.')
      }
      const actionAtStep = actions.find((action) => action.step === step)
      if (!actionAtStep) {
        throw new Error(`No action found for step ${step}.`)
      }
      if (actionAtStep.kind !== 'diff') {
        throw new Error(`Action at step ${step} is not a diff action.`)
      }
      return actionAtStep
    }
  )
  .parse(process.argv)
  .opts()

$.verbose = true

async function prepareApp() {
  await $`rm -rf ${appDir}`
  await waspNew(appDir)
  // TODO: Maybe we should have a whitelist of files we want to keep in src?
  await $`rm ${path.join(appDir, 'src/Main.css')}`
  await $`rm ${path.join(appDir, 'src/waspLogo.png')}`
  await $`rm ${path.join(appDir, 'src/MainPage.jsx')}`
  // Git needs to be initialized for patches to work
  await $`cd ${appDir} && git init`
}

await prepareApp()

if (editDiff) {
  const actionsBeforeStep = actions.filter(
    (action) => action.step < editDiff.step
  )
  await executeSteps(actionsBeforeStep, {
    untilStep: editDiff.step,
  })
  const actionsToEdit = actions
    .filter((action) => action.kind === 'diff')
    .filter(
      (action) => action.path === editDiff.path && action.step >= editDiff.step
    )

  // Okay, we are going to edit now all the steps that are related to the same file
  // starting with step editDiff.step
  console.log(
    `We are now going to edit all the steps for file ${editDiff.path} from step ${editDiff.step} onwards`
  )

  for (const action of actionsToEdit) {
    const { step, path } = action
    await makeCheckpoint()
    await Enquirer.prompt({
      type: 'confirm',
      name: 'edit',
      message: `Apply the new edit to ${path} at step ${step} and press Enter`,
      initial: true,
    })
    const patch = await generateGitPatch()
    console.log('=====================')
    console.log(patch)
    console.log('=====================')
  }
} else {
  await executeSteps(actions, {
    untilStep,
  })
}
