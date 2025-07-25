import { $ } from "zx";

import { log } from "./log";

export async function tagAllChanges(gitRepoDir: string, tagName: string) {
  await commitAllChanges(gitRepoDir, `Changes for tag: ${tagName}`);
  await $({ cwd: gitRepoDir })`git tag ${tagName}`;
  log("info", `Tagged changes with "${tagName}"`);
}

export async function generatePatchFromAllChanges(
  gitRepoDir: string,
): Promise<string> {
  await commitAllChanges(gitRepoDir, "temporary-commit");
  const patch = await generatePatchFromRevision(gitRepoDir, "HEAD");
  await removeLastCommit(gitRepoDir);
  return patch;
}

async function commitAllChanges(
  gitRepoDir: string,
  message: string,
): Promise<void> {
  await $({ cwd: gitRepoDir })`git add .`;
  await $({ cwd: gitRepoDir })`git commit -m "${message}"`;
}

async function removeLastCommit(gitRepoDir: string): Promise<void> {
  await $({ cwd: gitRepoDir })`git reset --hard HEAD~1`;
}

export async function generatePatchFromRevision(
  gitRepoDir: string,
  gitRevision: string,
): Promise<string> {
  const { stdout: patch } = await $({
    cwd: gitRepoDir,
  })`git show ${gitRevision} --format=`;

  return patch;
}

export async function initGitRepo(
  gitRepoDir: string,
  mainBranchName: string,
): Promise<void> {
  await $({ cwd: gitRepoDir })`git init`.quiet(true);
  await $({ cwd: gitRepoDir })`git branch -m ${mainBranchName}`;
  await $({ cwd: gitRepoDir })`git add .`;
  await $({ cwd: gitRepoDir })`git commit -m "Initial commit"`;
}
