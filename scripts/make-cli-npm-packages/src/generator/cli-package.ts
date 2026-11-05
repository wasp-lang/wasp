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

  const pkgSourceFilePath = path.join(CLI_PACKAGE_SOURCE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgSourceFilePath) as PackageJson;
  console.log(`Read package.json source: ${pkgSourceFilePath}`);

  pkg.name = packageName;
  pkg.version = packageVersion;

  pkg.optionalDependencies = {};
  for (const subPkgName of subPackages) {
    pkg.optionalDependencies[subPkgName.packageName] = packageVersion;
  }

  // No `os` and `cpu` here, or npm would refuse to install any Wasp project on
  // an unsupported platform. The platform sub-packages set them.

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
