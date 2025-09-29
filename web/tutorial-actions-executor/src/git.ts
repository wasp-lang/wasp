import { $ } from "zx";

export const mainBranchName = "main";

export async function generatePatchFromAllChanges(
  gitRepoDirPath: string,
): Promise<string> {
  await commitAllChanges(gitRepoDirPath, "temporary-commit");
  const patch = await generatePatchFromRevision(gitRepoDirPath, "HEAD");
  await removeLastCommit(gitRepoDirPath);
  return patch;
}

export async function applyPatch(gitRepoDirPath: string, patchPath: string) {
  await $({ cwd: gitRepoDirPath })`git apply ${patchPath} --verbose`.quiet(
    true,
  );
}

export async function findCommitSHAForExactMessage(
  gitRepoDirPath: string,
  message: string,
): Promise<string> {
  const commits = await grepGitCommitsByMessage(gitRepoDirPath, message);

  const commit = commits.find((commit) => commit.message === message);
  if (!commit) {
    throw new Error(`No commit found with message: "${message}"`);
  }

  return commit.sha;
}

interface GitCommit {
  message: string;
  sha: string;
}

async function grepGitCommitsByMessage(
  gitRepoDirPath: string,
  message: string,
): Promise<GitCommit[]> {
  const format = `{"message":"%s","sha":"%H"}`;
  const { stdout } = await $({
    cwd: gitRepoDirPath,
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
  gitRepoDirPath: string,
  message: string,
): Promise<void> {
  await $({ cwd: gitRepoDirPath })`git add .`;
  await $({ cwd: gitRepoDirPath })`git commit -m ${message}`;
}

async function removeLastCommit(gitRepoDirPath: string): Promise<void> {
  await $({ cwd: gitRepoDirPath })`git reset --hard HEAD~1`;
}

export async function generatePatchFromRevision(
  gitRepoDirPath: string,
  gitRevision: string,
): Promise<string> {
  const { stdout: patch } = await $({
    cwd: gitRepoDirPath,
  })`git show ${gitRevision} --format=`;

  return patch;
}

export async function initGitRepo(
  gitRepoDirPath: string,
  mainBranchName: string,
): Promise<void> {
  await $({ cwd: gitRepoDirPath })`git init`.quiet(true);
  await $({ cwd: gitRepoDirPath })`git branch -m ${mainBranchName}`;
}

export async function createBranchFromRevision({
  gitRepoDirPath,
  branchName,
  revision,
}: {
  gitRepoDirPath: string;
  branchName: string;
  revision: string;
}): Promise<void> {
  await $({
    cwd: gitRepoDirPath,
  })`git switch --force-create ${branchName} ${revision}`;
}

export async function moveLastCommitChangesToStaging(
  gitRepoDirPath: string,
): Promise<void> {
  await $({ cwd: gitRepoDirPath })`git reset --soft HEAD~1`;
}

export async function rebaseBranch({
  gitRepoDirPath,
  branchName,
  baseBranchName,
}: {
  gitRepoDirPath: string;
  branchName: string;
  baseBranchName: string;
}): Promise<void> {
  await $({ cwd: gitRepoDirPath })`git switch ${baseBranchName}`;
  await $({ cwd: gitRepoDirPath })`git rebase ${branchName}`;
}
