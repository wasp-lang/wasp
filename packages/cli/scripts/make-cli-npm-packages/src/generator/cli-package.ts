import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import {
  CLI_PACKAGE_SOURCE_DIR,
  CLI_PACKAGE_STATIC_FILES,
  copyStaticFiles,
} from "../common.ts";
import type { TarballData } from "../schema/input-data.ts";
import {
  UNDEFINED_LIBC_NAME,
  type CliPackageData,
} from "../schema/output-data.ts";

/** A platform package and the platform it holds the Wasp binary for. */
export interface CliSubPackage {
  packageName: string;
  target: { os: string; cpu: string; libc?: string };
}

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

  copyStaticFiles(
    CLI_PACKAGE_STATIC_FILES,
    CLI_PACKAGE_SOURCE_DIR,
    outputDirPath,
  );

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

  const pkg = readCliPackageJsonForPublishing();

  pkg.name = packageName;
  pkg.version = packageVersion;

  pkg.optionalDependencies = {};
  for (const subPkgName of subPackages) {
    pkg.optionalDependencies[subPkgName.packageName] = packageVersion;
  }

  // `os` and `cpu` stay on the platform sub-packages, or npm would refuse to
  // install any Wasp project on other platforms.

  console.log("Filled out package.json fields");
  console.groupEnd();

  return pkg;
}

/** The source `package.json`, without what only building from source needs. */
export function readCliPackageJsonForPublishing(): PackageJson {
  const pkgSourceFilePath = path.join(CLI_PACKAGE_SOURCE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgSourceFilePath) as PackageJson;
  console.log(`Read package.json source: ${pkgSourceFilePath}`);

  delete pkg.scripts;
  delete pkg.devDependencies;

  return pkg;
}

function writeDataFile(
  subPackages: { packageName: string; data: TarballData }[],
  outputDirPath: string,
) {
  console.group("Writing data file for CLI package...");

  const cliPackageData = makeCliPackageData(
    subPackages.map(({ packageName, data }) => ({
      packageName,
      target: data.target,
    })),
  );

  fs.writeJsonSync(path.join(outputDirPath, "data.json"), cliPackageData);
  console.log("Wrote data.json");

  console.groupEnd();
}

/** The `data.json` content, which tells the CLI the sub-package of each platform. */
export function makeCliPackageData(
  subPackages: CliSubPackage[],
): CliPackageData {
  const cliPackageData: CliPackageData = { subPackages: {} };
  for (const { packageName, target } of subPackages) {
    const { os, cpu, libc = UNDEFINED_LIBC_NAME } = target;
    cliPackageData.subPackages[os] ??= {};
    cliPackageData.subPackages[os][cpu] ??= {};
    cliPackageData.subPackages[os][cpu][libc] = { packageName };
  }
  return cliPackageData;
}
