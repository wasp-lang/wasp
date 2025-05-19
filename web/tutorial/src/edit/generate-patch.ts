import { $ } from 'zx'
import { appDir } from '../paths'

export async function makeCheckpoint(): Promise<void> {
  await $`cd ${appDir} && git add . && git commit -m "checkpoint"`.verbose(
    false
  )
}

export async function generateGitPatch(): Promise<string> {
  const { stdout: patch } = await $`cd ${appDir} && git diff`.verbose(false)
  return patch
}
