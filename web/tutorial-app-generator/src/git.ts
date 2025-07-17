import { $ } from "zx";

import { log } from "./log";

export async function tagChanges(gitRepoDir: string, tagName: string) {
  await $({ cwd: gitRepoDir })`git add .`;
  await $({ cwd: gitRepoDir })`git commit -m "${tagName}"`;
  await $({ cwd: gitRepoDir })`git tag ${tagName}`;
  log("info", `Tagged changes with "${tagName}"`);
}

export async function generatePatchFromChanges(
  gitRepoDir: string,
): Promise<string> {
  await $({ cwd: gitRepoDir })`git add .`;
  await $({ cwd: gitRepoDir })`git commit -m "temporary-commit"`;

  const patch = await getCommitPatch(gitRepoDir, "HEAD");

  await $({ cwd: gitRepoDir })`git reset --hard HEAD~1`;
  return patch;
}

export async function getCommitPatch(
  gitRepoDir: string,
  gitRevision: string,
): Promise<string> {
  const { stdout: patch } = await $({
    cwd: gitRepoDir,
  })`git show ${gitRevision} --format=`;

  return patch;
}
