/// <reference types="node" />

import { execa } from "execa";
import { getRepoRootPath, getWaspcDirPath } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";

const repoRootDir = getRepoRootPath();
const waspcDir = getWaspcDirPath();

export async function checkOrmolu(): Promise<void> {
  await runOrmolu("check");
}

export async function formatOrmolu(): Promise<void> {
  await runOrmolu("inplace");
}

export async function checkCabalGild(): Promise<void> {
  await runCabalGild("check");
}

export async function formatCabalGild(): Promise<void> {
  await runCabalGild("format");
}

export async function checkPrettier(): Promise<void> {
  await runCmd("pnpm", ["run", "check:prettier"], { cwd: repoRootDir });
}

export async function formatPrettier(): Promise<void> {
  await runCmd("pnpm", ["run", "format:prettier"], { cwd: repoRootDir });
}

async function runOrmolu(mode: "check" | "inplace"): Promise<void> {
  const haskellFiles = await listTrackedFiles(waspcDir, ["*.hs", "*.hs-boot"]);
  await runCmd(
    "ormolu",
    [
      "--color",
      "always",
      "--check-idempotence",
      "--mode",
      mode,
      ...haskellFiles,
    ],
    { cwd: waspcDir },
  );
}

async function runCabalGild(mode: "check" | "format"): Promise<void> {
  const cabalFiles = await listTrackedFiles(waspcDir, ["*.cabal"]);
  await runCmd("cabal-gild", ["--mode", mode, ...cabalFiles], {
    cwd: waspcDir,
  });
}

async function listTrackedFiles(
  dir: string,
  pathspecs: string[],
): Promise<string[]> {
  const { stdout } = await execa("git", ["ls-files", "-z", ...pathspecs], {
    cwd: dir,
  });
  return stdout.split("\0").filter(Boolean);
}
