import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { copyStaticFiles } from "../common.ts";
import type { TarballData } from "../schema/input-data.ts";
import {
  UNDEFINED_LIBC_NAME,
  type CliPackageData,
} from "../schema/output-data.ts";

// The CLI package (`@wasp/cli`) is a workspace package. This script turns it
// into the published one: it copies these files as they are and adds the
// version and the platform sub-packages to the package.json.
const CLI_PACKAGE_SOURCE_DIR = path.resolve(import.meta.dirname, "../../../..");
const COPIED_FILES = ["dist", "readme.md"];

const unique = <T>(array: Iterable<T>): T[] => Array.from(new Set(array));

export function makeCliPackage({
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
  console.group(`Creating CLI package ${packageName}...`);

  console.log(`Preparing CLI package directory: ${outputDirPath}`);
  fs.emptyDirSync(outputDirPath);

  copyStaticFiles(COPIED_FILES, CLI_PACKAGE_SOURCE_DIR, outputDirPath);

  const pkgJson = generatePackageJson(packageName, packageVersion, subPackages);
  const outPkgFilePath = path.join(outputDirPath, "package.json");
  fs.writeJsonSync(outPkgFilePath, pkgJson);
  console.log(`Wrote package.json to: ${outPkgFilePath}`);

  writeDataFile(subPackages, outputDirPath);

  console.log(`CLI package created at: ${outputDirPath}`);

  console.groupEnd();
}

function generatePackageJson(
  packageName: string,
  packageVersion: string,
  subPackages: { packageName: string; data: TarballData }[],
) {
  console.group("Generating package.json...");

  const pkgSourceFilePath = path.join(CLI_PACKAGE_SOURCE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgSourceFilePath) as PackageJson;
  console.log(`Read package.json source: ${pkgSourceFilePath}`);

  pkg.name = packageName;
  pkg.version = packageVersion;
  // Only needed to build the package from source.
  delete pkg.scripts;
  delete pkg.devDependencies;

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
  console.group("Writing data file for CLI package...");

  const pkgPaths = { subPackages: {} } as CliPackageData;
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
