import { $ } from "zx";
import { log } from "./log";

export async function commitAllChangesUnderTag(
  appDir: string,
  tagName: string,
) {
  await $`cd ${appDir} && git add . && git commit -m "${tagName}" && git tag ${tagName}`;
  log("info", `Tagged changes with "${tagName}"`);
}

export async function generatePatchFromChanges(
  appDir: string,
): Promise<string> {
  const { stdout: patch } = await $`cd ${appDir} &&
    git add . &&
    git commit -m "temporary-commit" &&
    git show --format=
    git reset --hard HEAD~1
    `.verbose(false);
  return patch;
}
