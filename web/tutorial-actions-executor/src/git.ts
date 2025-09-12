import { $ } from "zx";

export const mainBranchName = "main";

export async function generatePatchFromAllChanges(
  gitRepoDir: string,
): Promise<string> {
  await commitAllChanges(gitRepoDir, "temporary-commit");
  const patch = await generatePatchFromRevision(gitRepoDir, "HEAD");
  await removeLastCommit(gitRepoDir);
  return patch;
}

export async function applyPatch(gitRepoDir: string, patchPath: string) {
  await $({ cwd: gitRepoDir })`git apply ${patchPath} --verbose`.quiet(true);
}

export async function findCommitSHAForExactMessage(
  gitRepoDir: string,
  message: string,
): Promise<string> {
  const commits = await grepGitCommitMessages(gitRepoDir, message);

  const commit = commits.find((commit) => commit.message === message);
  if (!commit) {
    throw new Error(`No commit found with message: "${message}"`);
  }

  return commit.sha;
}

type GitCommit = {
  message: string;
  sha: string;
};

async function grepGitCommitMessages(
  gitRepoDir: string,
  message: string,
): Promise<GitCommit[]> {
  const format = `{"message":"%s","sha":"%H"}`;
  const { stdout } = await $({
    cwd: gitRepoDir,
  })`git log --branches --format=${format} --grep=${message}`;

  const commits = stdout.split("\n").filter((line) => line.trim() !== "");
  return commits.map((commit) => {
    const parsed = JSON.parse(commit);
    return {
      message: parsed.message,
      sha: parsed.sha,
    };
  });
}

export async function commitAllChanges(
  gitRepoDir: string,
  message: string,
): Promise<void> {
  await $({ cwd: gitRepoDir })`git add .`;
  await $({ cwd: gitRepoDir })`git commit -m ${message}`;
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

export async function createBranchFromRevision({
  gitRepoDir,
  branchName,
  revision,
}: {
  gitRepoDir: string;
  branchName: string;
  revision: string;
}): Promise<void> {
  await $({
    cwd: gitRepoDir,
  })`git switch --force-create ${branchName} ${revision}`;
}

export async function moveLastCommitChangesToStaging(
  gitRepoDir: string,
): Promise<void> {
  await $({ cwd: gitRepoDir })`git reset --soft HEAD~1`;
}

export async function rebaseBranch({
  gitRepoDir,
  branchName,
  baseBranchName,
}: {
  gitRepoDir: string;
  branchName: string;
  baseBranchName: string;
}): Promise<void> {
  await $({ cwd: gitRepoDir })`git switch ${baseBranchName}`;
  await $({ cwd: gitRepoDir })`git rebase ${branchName}`;
}
