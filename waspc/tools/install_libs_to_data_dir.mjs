// @ts-check

// Helper to compile the waspc/libs/* packages locally and in CI and then move
// them into the Cabal data dir.

import { execFileSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readdirSync,
  rmSync,
} from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const waspcDir = fileURLToPath(new URL("..", import.meta.url));
const dataLibsDir = join(waspcDir, "data", "Generator", "libs");

main();

async function main() {
  const waspcVersion = getWaspcVersion();
  console.log(`Wasp version: ${waspcVersion}`);

  cleanupOldLibs();
  await buildAndCopyLibs(waspcVersion);
}

function getWaspcVersion() {
  const scriptPath = fileURLToPath(
    new URL("./get-waspc-version.mjs", import.meta.url),
  );
  return runCmd("node", [scriptPath]).trim();
}

function cleanupOldLibs() {
  if (existsSync(dataLibsDir)) {
    rmSync(dataLibsDir, { recursive: true, force: true });
  }
  mkdirSync(dataLibsDir, { recursive: true });
}

async function buildAndCopyLibs(/** @type {string} */ waspcVersion) {
  const libsDir = join(waspcDir, "libs");
  const libDirs = readdirSync(libsDir, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => join(libsDir, dirent.name));

  for (const libDir of libDirs) {
    await buildAndCopyLib(libDir, waspcVersion);
  }
}

async function buildAndCopyLib(
  /** @type {string} */ libDir,
  /** @type {string} */ waspcVersion,
) {
  const packageJsonPath = join(libDir, "package.json");
  const packageJson = JSON.parse(await readFile(packageJsonPath, "utf-8"));
  const libName = packageJson.name;
  const libVersion = packageJson.version;

  console.log(`Installing ${libName} lib (${libDir})`);

  if (libVersion !== waspcVersion) {
    console.error(
      `ERROR: ${libName} lib version (${libVersion}) != current Wasp version (${waspcVersion}).`,
    );
    console.error(
      `       Update the lib version in package.json to ${waspcVersion}.`,
    );
    throw new Error();
  }

  runCmd("npm", ["install"], { cwd: libDir });

  // Clean up old lib tarballs.
  const oldTarballs = readdirSync(libDir).filter((f) => f.endsWith(".tgz"));
  for (const tarball of oldTarballs) {
    rmSync(join(libDir, tarball));
  }

  runCmd("npm", ["pack"], { cwd: libDir });

  // Copy the new tarball to data dir.
  const newTarballs = readdirSync(libDir).filter((f) => f.endsWith(".tgz"));
  for (const tarball of newTarballs) {
    copyFileSync(join(libDir, tarball), join(dataLibsDir, tarball));
  }
}

function runCmd(
  /** @type {string} */ cmd,
  /** @type {string[]} */ args,
  { cwd = waspcDir } = {},
) {
  return execFileSync(cmd, args, {
    cwd,
    encoding: "utf-8",
    shell: true,
    stdio: ["ignore", "inherit", "inherit"],
  });
}
