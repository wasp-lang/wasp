/**
 * Guards against drift in the generated markdown docs and `llms*.txt` files.
 * The snapshots track the "current" (next, unreleased) docs version so
 * drift shows up in the PR that changes the docs, not at release time.
 *
 * These files only exist in `build/` and can't be tracked by git directly.
 * Instead we keep snapshots of a few representative files in
 * `markdown-snapshots/` and compare against them.
 *
 * Usage: node ./scripts/markdown-snapshots.mts <check|update>
 *
 * Run `npm run build-dev` first, then `update` to (re)create the snapshots
 * or `check` to compare the build output against them.
 */

import assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { parseArgs } from "node:util";

const { positionals } = parseArgs({
  options: {},
  strict: true,
  allowPositionals: true,
});
assert(
  positionals.length === 1 && ["check", "update"].includes(positionals[0]),
  "node markdown-snapshots.mts <check|update>",
);

const [MODE] = positionals;
const WEB_ROOT = path.resolve(import.meta.dirname, "..");
const BUILD_DIR = path.join(WEB_ROOT, "build");
const SNAPSHOTS_DIR = path.join(WEB_ROOT, "markdown-snapshots");
/**
 * Docusaurus's name for the "next" (unreleased) docs version.
 * Included in the build only when `DOCS_INCLUDE_CURRENT_VERSION=true`
 */
const NEXT_DOCS_VERSION = "current";

/**
 * Lists relative paths of all different file "situations" we want
 * to track in markdown snapshot tests.
 */
const SNAPSHOT_REL_PATHS = [
  // Universal index, including the blog and resources post collections.
  "llms.txt",
  // Docs index for the next docs version.
  `llms-${NEXT_DOCS_VERSION}.txt`,
  // Full docs content of the "next" docs version.
  // Renders markdown with some additional transformations.
  "llms-full.txt",
  // A docs page. Renders markdown normally.
  "docs/auth/overview.md",
  // A docs guide page. Renders markdown normally.
  "docs/guides/legacy/installer.md",
  // An API docs index page.
  "docs/api/@wasp.sh/spec.md",
  // A blog post.
  "blog/2023/06/27/build-your-own-twitter-agent-langchain.md",
  // A resources post.
  "resources/2026/02/24/best-frameworks-web-dev-2026.md",
];

await assertBuildIncludesNextDocsVersion();

if (MODE === "check") {
  await checkSnapshots();
} else {
  await updateSnapshots();
}

async function assertBuildIncludesNextDocsVersion(): Promise<void> {
  assert(
    await fileExists(path.join(BUILD_DIR, "llms.txt")),
    "No build output found. Run `npm run build-dev` first.",
  );
  assert(
    await fileExists(path.join(BUILD_DIR, `llms-${NEXT_DOCS_VERSION}.txt`)),
    "Build output does not include the next docs version. Rebuild with `npm run build-dev`.",
  );
}

async function checkSnapshots(): Promise<void> {
  const results = await Promise.allSettled(
    SNAPSHOT_REL_PATHS.map(checkSnapshot),
  );
  const errors = results
    .filter((result) => result.status === "rejected")
    .map((result) => result.reason);

  if (errors.length > 0) {
    throw new AggregateError(
      errors,
      "Markdown output drifted from the snapshots. You can preview the changes by running `npm run markdown-snapshots:update`. Inspect the git diffs, and commit the results if they are intended.",
    );
  }

  console.log(`All ${SNAPSHOT_REL_PATHS.length} markdown snapshots match.`);
}

async function checkSnapshot(snapshotRelPath: string): Promise<void> {
  const builtFile = await readFileOrNull(path.join(BUILD_DIR, snapshotRelPath));
  const snapshotFile = await readFileOrNull(
    path.join(SNAPSHOTS_DIR, snapshotRelPath),
  );

  if (builtFile === null) {
    throw new Error(
      `missing file in build output (stale snapshot?): ${snapshotRelPath}`,
    );
  } else if (snapshotFile === null) {
    throw new Error(`missing file in snapshots: ${snapshotRelPath}`);
  } else if (builtFile !== snapshotFile) {
    throw new Error(
      `built file differs from snapshot file: ${snapshotRelPath}`,
    );
  }
}

async function updateSnapshots(): Promise<void> {
  await fs.rm(SNAPSHOTS_DIR, { recursive: true, force: true });

  for (const snapshotRelPath of SNAPSHOT_REL_PATHS) {
    const snapshotContent = await readFileOrNull(
      path.join(BUILD_DIR, snapshotRelPath),
    );
    assert(
      snapshotContent !== null,
      `Missing "${snapshotRelPath}" in build output. Run \`npm run build-dev\` first, or update the snapshot list.`,
    );

    const snapshotFilePath = path.join(SNAPSHOTS_DIR, snapshotRelPath);

    await fs.mkdir(path.dirname(snapshotFilePath), { recursive: true });
    await fs.writeFile(snapshotFilePath, snapshotContent);
  }

  console.log(`Updated ${SNAPSHOT_REL_PATHS.length} markdown snapshots.`);
  console.log("Review the changes with git before committing them.");
}

async function fileExists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function readFileOrNull(filePath: string): Promise<string | null> {
  try {
    return await fs.readFile(filePath, "utf-8");
  } catch {
    return null;
  }
}
