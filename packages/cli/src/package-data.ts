// The format of `data.json`, which lists the platform packages that hold the
// Wasp binary, the API each platform package exports, and how we pick ours.

import { report as processReport } from "node:process";

export interface CliSubPackageInfo {
  packageName: string;
}

// This can be any string representing a lack of a defined libc for the build
// (e.g. macOS or Windows don't use libc in the same way Linux does).
// The usual convention is to call it "unknown".
export const UNDEFINED_LIBC_NAME = "unknown" as const;

export interface CliPackageData {
  subPackages: {
    [os: string]: {
      [arch: string]: {
        [libc: string | typeof UNDEFINED_LIBC_NAME]: CliSubPackageInfo;
      };
    };
  };
}

export interface CliSubPackageAPI {
  waspBinPath: string;
  dataDirPath: string;
}

export interface PlatformTarget {
  os: string;
  cpu: string;
  libc: string;
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
