import { $ } from "zx";

export async function makeCheckpoint(appDir: string): Promise<void> {
  await $`cd ${appDir} && git add . && git commit -m "checkpoint"`.verbose(
    false,
  );
}

export async function generateGitPatch(appDir: string): Promise<string> {
  const { stdout: patch } = await $`cd ${appDir} && git diff`.verbose(false);
  return patch;
}
