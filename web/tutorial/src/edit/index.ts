import fs from 'fs/promises'
import Enquirer from 'enquirer'

import type { Action, ApplyPatchAction } from '../actions'
import { executeSteps } from '../execute-steps'
import { generateGitPatch, makeCheckpoint } from './generate-patch'
import { log } from '../log'

export async function updateBrokenDiffs(
  editDiff: ApplyPatchAction,
  actions: Action[]
) {
  const actionsBeforeStep = actions.filter(
    (action) => action.step < editDiff.step
  )
  await executeSteps(actionsBeforeStep, {
    untilStep: editDiff.step,
  })
  const diffActionsForSameFile = actions
    .filter((action) => action.kind === 'diff')
    .filter(
      (action) =>
        action.targetFilePath === editDiff.targetFilePath &&
        action.step >= editDiff.step
    )

  log(
    'info',
    `We are now going to edit all the steps for file ${editDiff.targetFilePath} from step ${editDiff.step} onwards`
  )

  for (const action of diffActionsForSameFile) {
    const { step, targetFilePath, patchContentPath } = action
    await makeCheckpoint()
    await Enquirer.prompt({
      type: 'confirm',
      name: 'edit',
      message: `Apply the new edit to ${targetFilePath} at step ${step} and press Enter`,
      initial: true,
    })
    const patch = await generateGitPatch()
    await fs.writeFile(patchContentPath, patch, 'utf-8')
    log('info', `Patch for step ${step} written to ${patchContentPath}.`)
  }
}
