import type { ActionCommon, ApplyPatchAction } from './index'
import parseGitDiff from 'parse-git-diff'

export function createApplyPatchAction(
  patch: string,
  commonActionData: ActionCommon
): ApplyPatchAction {
  const parsedPatch = parseGitDiff(patch)

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error('Invalid patch: no changes found')
  }

  if (parsedPatch.files.length > 1) {
    throw new Error('Invalid patch: multiple files changed')
  }

  if (parsedPatch.files[0].type !== 'ChangedFile') {
    throw new Error('Invalid patch: only file changes are supported')
  }

  const path = parsedPatch.files[0].path

  return {
    ...commonActionData,
    kind: 'diff',
    patch,
    path,
  }
}
