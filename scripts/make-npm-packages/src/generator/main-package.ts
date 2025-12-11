import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { COMMON_TEMPLATE_DIR } from "../common.ts";
import type { TarballData } from "../schema/input-data.ts";
import type { MainPackageData } from "../schema/output-data.ts";

const TEMPLATE_DIR = path.join(COMMON_TEMPLATE_DIR, "main-package");
const TEMPLATE_COPY_FILES = ["bin.js", "CLIError.js"];

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

  console.log("Preparing main package directory:", outputDirPath);
  fs.emptyDirSync(outputDirPath);

  copyStaticFiles(outputDirPath, TEMPLATE_COPY_FILES);

  const pkgJson = generatePackageJson(packageName, packageVersion, subPackages);
  const outPkgFilePath = path.join(outputDirPath, "package.json");
  fs.writeJsonSync(outPkgFilePath, pkgJson);
  console.log("Wrote package.json to:", outPkgFilePath);

  writeDataFile(subPackages, outputDirPath);

  console.log("Main package created at:", outputDirPath);

  console.groupEnd();
}

function copyStaticFiles(outputDirPath: string, fileNames: string[]) {
  console.group("Copying template files...");

  for (const fileName of fileNames) {
    fs.copyFileSync(
      path.join(TEMPLATE_DIR, fileName),
      path.join(outputDirPath, fileName),
    );
    console.log(`Copied ${fileName}`);
  }

  console.groupEnd();
}

function generatePackageJson(
  packageName: string,
  packageVersion: string,
  subPackages: { packageName: string; data: TarballData }[],
) {
  console.group("Generating package.json...");

  const pkgTemplateFilePath = path.join(TEMPLATE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgTemplateFilePath) as PackageJson;
  console.log("Read package.json template:", pkgTemplateFilePath);

  pkg.name = packageName;
  pkg.version = packageVersion;

  pkg.optionalDependencies = {};
  for (const subPkgName of subPackages)
    pkg.optionalDependencies[subPkgName.packageName] = packageVersion;

  pkg.os = unique(subPackages.map((subPackage) => subPackage.data.target[0]));
  pkg.cpu = unique(subPackages.map((subPackage) => subPackage.data.target[1]));

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
    const [os, arch, libc = "unknown"] = subPackage.data.target;
    pkgPaths.subPackages[os] ??= {};
    pkgPaths.subPackages[os][arch] ??= {};
    pkgPaths.subPackages[os][arch][libc] = {
      packageName: subPackage.packageName,
    };
  }

  fs.writeJsonSync(path.join(outputDirPath, "data.json"), pkgPaths);
  console.log("Wrote data.json");

  console.groupEnd();
}
