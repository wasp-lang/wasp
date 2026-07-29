/// <reference types="node" />
// Generates the package-lock.json files we ship with the bundled starter
// templates, so that the initial `npm install` in a new Wasp project finds a
// pre-resolved dependency tree (faster install, pinned known-good versions).
//
// For each bundled starter, this script scaffolds a project in a temp dir the
// same way `wasp new` + `wasp install` do, runs the real `npm install`, and
// copies the resulting lockfile back into the starter template dir.
//
// Modes:
//   (default)  Regenerate the lockfiles. Existing committed lockfiles seed the
//              install, so already-pinned versions are kept and only actual
//              changes (starter package.json, spec package, ...) are picked up.
//   --check    Regenerate in a temp dir and fail if the result differs from
//              the committed lockfiles (used in CI).
//   --refresh  Regenerate from scratch, re-pinning all deps to the latest
//              versions allowed by the starter package.json ranges.

import {
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";
import { getWaspcDirPath, runCmd } from "../utils.ts";
import { getBundledStarterDirPaths, getSkeletonDirPath } from "./utils.ts";

// The app name placeholder is not a valid npm package name (leading
// underscore), so we scaffold with a valid sentinel name and swap the
// placeholder back into the generated lockfile afterwards.
const SENTINEL_APP_NAME = "wasp-starter-lockfile-sentinel";

// Must match the placeholders replaced by `wasp new`, see
// `waspc/cli/src/Wasp/Cli/Command/CreateNewProject/StarterTemplates/Templating.hs`.
const APP_NAME_PLACEHOLDER = "__waspAppName__";
const SPEC_SPECIFIER_PLACEHOLDER = "__waspSpecPackageSpecifier__";

// Must match `getInstallablePackageName` and `getPackageJsonSpecifierForPackage`
// in `waspc/src/Wasp/NodePackageFFI.hs`. The trailing slash comes from
// StrongPath's dir-to-string conversion.
const SPEC_PACKAGE_NAME = "@wasp.sh/spec";
const SPEC_PACKAGE_SPECIFIER = "file:.wasp/spec/";

// Must match `skeletonDotfiles` in
// `waspc/cli/src/Wasp/Cli/Command/CreateNewProject/StarterTemplates.hs`.
const SKELETON_DOTFILES = ["gitignore", "npmrc"];

const NPM_REGISTRY_PREFIX = "https://registry.npmjs.org/";

const PACKAGE_LOCK_FILE_NAME = "package-lock.json";

type Mode = "generate" | "check" | "refresh";

type PackageLock = {
  name: string;
  packages: Record<string, PackageLockEntry>;
};

type PackageLockEntry = {
  name?: string;
  devDependencies?: Record<string, string>;
  resolved?: string;
};

try {
  main();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function main(): void {
  const mode = parseMode(process.argv.slice(2));

  const staleStarterNames: string[] = [];
  for (const starterDirPath of getBundledStarterDirPaths()) {
    const starterName = basename(starterDirPath);
    console.log(`Generating lockfile for the "${starterName}" starter...`);

    const lockfileContent = generateLockfileForStarter(starterDirPath, mode);
    const committedLockfilePath = join(starterDirPath, PACKAGE_LOCK_FILE_NAME);

    if (mode === "check") {
      if (!isFileWithContent(committedLockfilePath, lockfileContent)) {
        staleStarterNames.push(starterName);
      }
    } else {
      writeFileSync(committedLockfilePath, lockfileContent);
      console.log(`Wrote ${committedLockfilePath}`);
    }
  }

  if (staleStarterNames.length > 0) {
    throw new Error(
      `Starter lockfiles are out of date for: ${staleStarterNames.join(", ")}. ` +
        "Run `./run update:starter-lockfiles` and commit the result.",
    );
  }
}

function parseMode(args: string[]): Mode {
  const unknownArgs = args.filter(
    (arg) => arg !== "--check" && arg !== "--refresh",
  );
  if (unknownArgs.length > 0) {
    throw new Error(
      `Unknown arguments: ${unknownArgs.join(" ")}. Usage: generate-lockfiles.ts [--check | --refresh]`,
    );
  }
  if (args.includes("--check") && args.includes("--refresh")) {
    throw new Error("--check and --refresh are mutually exclusive.");
  }
  if (args.includes("--check")) return "check";
  if (args.includes("--refresh")) return "refresh";
  return "generate";
}

function generateLockfileForStarter(
  starterDirPath: string,
  mode: Mode,
): string {
  const tempProjectDirPath = mkdtempSync(join(tmpdir(), "wasp-starter-"));
  try {
    scaffoldProject(starterDirPath, tempProjectDirPath, mode);
    runCmd("npm", ["install", "--no-audit", "--no-fund"], {
      cwd: tempProjectDirPath,
      stdio: "inherit",
    });
    return postProcessLockfile(
      join(tempProjectDirPath, PACKAGE_LOCK_FILE_NAME),
    );
  } finally {
    rmSync(tempProjectDirPath, { recursive: true, force: true });
  }
}

// Mirrors the project scaffolding done by `wasp new` + `wasp install`: see
// `createProjectOnDiskFromBundledTemplate` in
// `waspc/cli/src/Wasp/Cli/Command/CreateNewProject/StarterTemplates/Bundled.hs`
// and `installIO` in `waspc/cli/src/Wasp/Cli/Command/Install.hs`.
function scaffoldProject(
  starterDirPath: string,
  projectDirPath: string,
  mode: Mode,
): void {
  cpSync(getSkeletonDirPath(), projectDirPath, { recursive: true });
  for (const dotfile of SKELETON_DOTFILES) {
    renameSync(
      join(projectDirPath, dotfile),
      join(projectDirPath, `.${dotfile}`),
    );
  }

  cpSync(starterDirPath, projectDirPath, {
    recursive: true,
    force: true,
    filter: (src) => basename(src) !== PACKAGE_LOCK_FILE_NAME,
  });

  replacePlaceholdersInFile(join(projectDirPath, "package.json"));

  // Seed the install with the committed lockfile so regeneration is
  // idempotent: npm keeps locked versions that still satisfy package.json.
  // With --refresh there is no seed, so all deps re-resolve to the latest
  // versions their ranges allow.
  const committedLockfilePath = join(starterDirPath, PACKAGE_LOCK_FILE_NAME);
  if (mode !== "refresh" && existsSync(committedLockfilePath)) {
    cpSync(committedLockfilePath, join(projectDirPath, PACKAGE_LOCK_FILE_NAME));
    replacePlaceholdersInFile(join(projectDirPath, PACKAGE_LOCK_FILE_NAME));
  }

  const specSrcDirPath = join(getWaspcDirPath(), "data", "packages", "spec");
  const specDestDirPath = join(projectDirPath, ".wasp", "spec");
  mkdirSync(specDestDirPath, { recursive: true });
  cpSync(specSrcDirPath, specDestDirPath, {
    recursive: true,
    // Only package.json matters for dependency resolution.
    filter: (src) =>
      basename(src) !== "node_modules" && basename(src) !== "dist",
  });
}

function replacePlaceholdersInFile(filePath: string): void {
  const content = readFileSync(filePath, "utf-8");
  writeFileSync(
    filePath,
    content
      .replaceAll(APP_NAME_PLACEHOLDER, SENTINEL_APP_NAME)
      .replaceAll(SPEC_SPECIFIER_PLACEHOLDER, SPEC_PACKAGE_SPECIFIER),
  );
}

// Swaps the sentinel app name back for the placeholder and sanity-checks the
// lockfile before it gets committed into the starter template.
function postProcessLockfile(lockfilePath: string): string {
  const lock: PackageLock = JSON.parse(readFileSync(lockfilePath, "utf-8"));

  assert(
    lock.name === SENTINEL_APP_NAME,
    `Expected lockfile name to be "${SENTINEL_APP_NAME}", got "${lock.name}".`,
  );
  lock.name = APP_NAME_PLACEHOLDER;

  const rootEntry = lock.packages[""];
  assert(
    rootEntry !== undefined && rootEntry.name === SENTINEL_APP_NAME,
    `Expected root package entry name to be "${SENTINEL_APP_NAME}".`,
  );
  rootEntry.name = APP_NAME_PLACEHOLDER;

  assert(
    rootEntry.devDependencies?.[SPEC_PACKAGE_NAME] === SPEC_PACKAGE_SPECIFIER,
    `Expected root package entry to have the "${SPEC_PACKAGE_NAME}": "${SPEC_PACKAGE_SPECIFIER}" dev dependency.`,
  );

  for (const [entryPath, entry] of Object.entries(lock.packages)) {
    const resolved = entry.resolved;
    assert(
      resolved === undefined ||
        !resolved.includes("://") ||
        resolved.startsWith(NPM_REGISTRY_PREFIX),
      `Package "${entryPath}" resolves to "${resolved}" instead of the public npm registry. ` +
        "Is a custom registry or mirror configured in your npm config?",
    );
  }

  // npm's own on-disk format, so later `npm install` runs in user projects
  // keep the file byte-identical.
  return JSON.stringify(lock, null, 2) + "\n";
}

function isFileWithContent(filePath: string, content: string): boolean {
  return existsSync(filePath) && readFileSync(filePath, "utf-8") === content;
}

function assert(condition: boolean, message: string): asserts condition {
  if (!condition) {
    throw new Error(message);
  }
}
