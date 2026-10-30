import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { copyStaticFiles } from "../common.ts";
import type { TarballData } from "../schema/input-data.ts";
import {
  UNDEFINED_LIBC_NAME,
  type MainPackageData,
} from "../schema/output-data.ts";

// The main package (`@wasp/cli`) is a workspace package. This script turns it
// into the published one: it copies these files as they are and adds the
// version and the platform sub-packages to the package.json.
const MAIN_PACKAGE_SOURCE_DIR = path.resolve(
  import.meta.dirname,
  "../../../../packages/cli",
);
const COPIED_FILES = ["bin.js", "CLIError.js", "readme.md"];

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

  copyStaticFiles(COPIED_FILES, MAIN_PACKAGE_SOURCE_DIR, outputDirPath);

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

  const pkgSourceFilePath = path.join(MAIN_PACKAGE_SOURCE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgSourceFilePath) as PackageJson;
  console.log(`Read package.json source: ${pkgSourceFilePath}`);

  pkg.name = packageName;
  pkg.version = packageVersion;

  pkg.optionalDependencies = {};
  for (const subPkgName of subPackages) {
    pkg.optionalDependencies[subPkgName.packageName] = packageVersion;
  }

  // No `os` / `cpu` on the main package: it is a dependency of every Wasp
  // project (through `@wasp/wasp`), and npm would refuse to install the whole
  // project on an unsupported platform. The platform packages carry them, and
  // `bin.js` explains the problem when none of them is installed.

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
