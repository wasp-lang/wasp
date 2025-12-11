import { execaSync } from "execa";
import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { COMMON_TEMPLATE_DIR } from "../common.ts";
import type { Path } from "../schema/util.ts";

const TEMPLATE_DIR = path.join(COMMON_TEMPLATE_DIR, "sub-package");
const TEMPLATE_COPY_FILES = ["main.js"];

export const makeSubPackage = ({
  packageName,
  packageVersion,
  tarballPath,
  target,
  outputDirPath,
}: {
  packageName: string;
  packageVersion: string;
  tarballPath: Path;
  target: { os: string; cpu: string; libc?: string };
  outputDirPath: string;
}) => {
  console.group(`Creating subpackage ${packageName}...`);

  console.log("Preparing subpackage directory:", outputDirPath);
  fs.emptyDirSync(outputDirPath);

  copyStaticFiles(outputDirPath, TEMPLATE_COPY_FILES);

  const pkg = generatePackageJson(packageName, packageVersion, target);
  const outPkgFilePath = path.join(outputDirPath, "package.json");
  fs.writeJsonSync(outPkgFilePath, pkg);
  console.log("Wrote package.json to:", outPkgFilePath);

  console.log("Extracting tarball contents...");
  execaSync`tar -xzvf ${tarballPath} -C ${outputDirPath}`;

  console.log("Subpackage created at:", outputDirPath);

  console.groupEnd();
};

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
  target: { os: string; cpu: string; libc?: string },
) {
  console.group("Creating package.json...");

  const pkgTemplateFilePath = path.join(TEMPLATE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgTemplateFilePath) as PackageJson;
  console.log("Read package.json template:", pkgTemplateFilePath);

  pkg.name = packageName;
  pkg.version = packageVersion;
  pkg.os = [target.os];
  pkg.cpu = [target.cpu];
  if (target.libc) pkg.libc = [target.libc];

  console.log("Filled out package.json fields");
  console.groupEnd();

  return pkg;
}
