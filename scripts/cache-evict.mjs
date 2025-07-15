// @ts-check

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";

const { repository } = assertGithubActionsEnv();
deleteOrphanedCaches(repository);

function deleteOrphanedCaches(/** @type {string} */ githubRepository) {
  const ghCachesOutput = listGitHubCaches();
  const lsRemoteOutput = listRemoteRefs(githubRepository);

  // Find refs that have caches but do not exist in the remote repository anymore.
  const remoteRefs = new Set(lsRemoteOutput.map(([, ref]) => ref));
  const cachesToDelete = ghCachesOutput.filter(
    (cache) => !remoteRefs.has(cache.ref),
  );
  console.log(`Found ${cachesToDelete.length} caches to delete`);

  // Delete those cache keys
  for (const { key, ref } of cachesToDelete) {
    try {
      console.group(`Deleting cache "${key}" for ref "${ref}"`);
      runCmd("gh", ["cache", "delete", key], { collectStdout: false });
      console.log(`Done`);
    } catch (e) {
      console.warn(`::warning::Failed to delete cache key ${key}`);
    } finally {
      console.groupEnd();
    }
  }

  console.log("Done");
}

function listGitHubCaches() {
  const ghCachesOutput = /** @type {{ key: string, ref: string }[]} */ (
    JSON.parse(
      runCmd(
        // We ask for the output to be a JSON array of {key, ref} objects.
        "gh",
        ["cache", "list", "--limit", "100", "--json", "key,ref"],
      ),
    )
  );
  console.log(`Found ${ghCachesOutput.length} cache keys`);
  return ghCachesOutput;
}

function listRemoteRefs(githubRepository) {
  const lsRemoteOutput = /** @type {[objectId: string, ref: string][]} */ (
    parseTabSeparatedLines(
      runCmd(
        // We use `git ls-remote` so we also receive refs such as `refs/pull/123/head`
        // which are not downloaded by `git fetch` or `git pull`.
        //
        // According to https://git-scm.com/docs/git-ls-remote, the output is:
        // <oid> TAB <ref> LF
        "git",
        ["ls-remote", `https://github.com/${githubRepository}.git`],
      ),
    )
  );
  console.log(`Found ${lsRemoteOutput.length} remote refs`);
  return lsRemoteOutput;
}

function assertGithubActionsEnv() {
  const GITHUB_REPOSITORY = process.env.GITHUB_REPOSITORY;
  assert(
    GITHUB_REPOSITORY,
    "GITHUB_REPOSITORY environment variable is required. This environment variable is typically set by GitHub Actions.",
  );

  const ghVersion = runCmd("gh", ["--version"]).trim();
  const gitVersion = runCmd("git", ["--version"]).trim();

  console.group("Environment");
  console.log(`GITHUB_REPOSITORY: ${GITHUB_REPOSITORY}`);
  console.log(`gh version: ${ghVersion}`);
  console.log(`git version: ${gitVersion}`);
  console.groupEnd();

  return { repository: GITHUB_REPOSITORY, ghVersion, gitVersion };
}

function runCmd(
  /** @type {string} */ cmd,
  /** @type {string[]} */ args,
  { collectStdout = true } = {},
) {
  return execFileSync(cmd, args, {
    encoding: "utf-8",
    stdio:
      // stdin, stdout, stderr
      ["ignore", collectStdout ? "pipe" : "inherit", "inherit"],
  });
}

function parseTabSeparatedLines(/** @type {string} */ str) {
  return str
    .trim()
    .split("\n")
    .map((line) => line.split("\t"));
}
