import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { MAIN_PACKAGE_SOURCE_DIR, copyStaticFiles } from "../common.ts";
import type { TarballData } from "../schema/input-data.ts";
import {
  UNDEFINED_LIBC_NAME,
  type MainPackageData,
} from "../schema/output-data.ts";

const SOURCE_DIR = MAIN_PACKAGE_SOURCE_DIR;
const SOURCE_STATIC_FILES = [
  "bin.js",
  "CLIError.js",
  "readme.md",
  "preinstall.js",
  "postinstall.js",
];

const unique = <T>(array: Iterable<T>): T[] => Array.from(new Set(array));

export function makeMainPackage({
  packageName,
  packageVersion,
  subPackages,
  outputDirPath,
}: {
  packageName: string;
  packageVersion: string;
  subPackages: { packageName: string; data: TarballData }[];
  outputDirPath: string;
}) {
  console.group(`Creating main package ${packageName}...`);

  console.log(`Preparing main package directory: ${outputDirPath}`);
  fs.emptyDirSync(outputDirPath);

  copyStaticFiles(SOURCE_STATIC_FILES, SOURCE_DIR, outputDirPath);

  const pkgJson = generatePackageJson(packageName, packageVersion, subPackages);
  const outPkgFilePath = path.join(outputDirPath, "package.json");
  fs.writeJsonSync(outPkgFilePath, pkgJson);
  console.log(`Wrote package.json to: ${outPkgFilePath}`);

  writeDataFile(subPackages, outputDirPath);

  console.log(`Main package created at: ${outputDirPath}`);

  console.groupEnd();
}

function generatePackageJson(
  packageName: string,
  packageVersion: string,
  subPackages: { packageName: string; data: TarballData }[],
) {
  console.group("Generating package.json...");

  const pkgSourceFilePath = path.join(SOURCE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgSourceFilePath) as PackageJson;
  console.log(`Read package.json source: ${pkgSourceFilePath}`);

  pkg.name = packageName;
  pkg.version = packageVersion;

  // The install scripts talk to the user's machine (installer conflict check,
  // telemetry), so they belong to the published package only, not to the
  // workspace package that every `pnpm install` in this repo would run.
  pkg.scripts = {
    preinstall: "node preinstall.js",
    postinstall: "node postinstall.js",
  };

  pkg.optionalDependencies = {};
  for (const subPkgName of subPackages) {
    pkg.optionalDependencies[subPkgName.packageName] = packageVersion;
  }

  pkg.os = unique(subPackages.map((subPackage) => subPackage.data.target.os));
  pkg.cpu = unique(subPackages.map((subPackage) => subPackage.data.target.cpu));

  console.log("Filled out package.json fields");
  console.groupEnd();

  return pkg;
}

function writeDataFile(
  subPackages: { packageName: string; data: TarballData }[],
  outputDirPath: string,
) {
  console.group("Writing data file for main package...");

  const pkgPaths = { subPackages: {} } as MainPackageData;
  for (const subPackage of subPackages) {
    const { os, cpu, libc = UNDEFINED_LIBC_NAME } = subPackage.data.target;
    pkgPaths.subPackages[os] ??= {};
    pkgPaths.subPackages[os][cpu] ??= {};
    pkgPaths.subPackages[os][cpu][libc] = {
      packageName: subPackage.packageName,
    };
  }

  fs.writeJsonSync(path.join(outputDirPath, "data.json"), pkgPaths);
  console.log("Wrote data.json");

  console.groupEnd();
}
