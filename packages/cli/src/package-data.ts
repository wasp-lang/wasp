// Picks the sub-package that holds the Wasp binary for a platform.
//
// `scripts/make-cli-npm-packages` and `mise-tasks/build/dev-packages.ts` run this
// file with Node's type stripping, so it can only import types and Node.js
// built-ins.

import { report as processReport } from "node:process";
import type {
  CliPackageData,
  CliSubPackageInfo,
  PlatformTarget,
} from "wasp-cli-package-data-types";

// This can be any string representing a lack of a defined libc for the build
// (e.g. macOS or Windows don't use libc in the same way Linux does).
// The usual convention is to call it "unknown".
export const UNDEFINED_LIBC_NAME = "unknown" as const;

/** `undefined` if there is no sub-package for the platform. */
export function findSubPackageInfo(
  packageData: CliPackageData,
  { os, cpu, libc }: PlatformTarget,
): CliSubPackageInfo | undefined {
  return packageData.subPackages[os]?.[cpu]?.[libc];
}

/** The `data.json` keys of the platform this process runs on. */
export function getCurrentPlatformTarget(): PlatformTarget {
  const { platform: os, arch: cpu } = process;
  const libc =
    os === "linux" ? (isGlibc() ? "glibc" : "musl") : UNDEFINED_LIBC_NAME;
  return { os, cpu, libc };
}

// Adapted from rollup
// https://github.com/rollup/rollup/blob/c5f3e1d3162ccb36e18704138c21457a084ef358/native.js#L5-L11
function isGlibc(): boolean {
  try {
    const header = (
      processReport.getReport() as { header?: { glibcVersionRuntime?: string } }
    ).header;
    return Boolean(header?.glibcVersionRuntime);
  } catch {
    return false;
  }
}
