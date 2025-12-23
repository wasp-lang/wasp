import { execaSync } from "execa";
import fs from "fs-extra";
import * as path from "node:path";
import type { PackageJson } from "type-fest";
import { TEMPLATES_DIR, copyStaticFiles } from "../common.ts";
import type { NpmTarget } from "../schema/input-data.ts";
import { UNDEFINED_LIBC_NAME } from "../schema/output-data.ts";
import type { Path } from "../schema/util.ts";

const TEMPLATE_DIR = path.join(TEMPLATES_DIR, "sub-package");
const TEMPLATE_STATIC_FILES = ["main.js", "readme.md"];

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
  target: NpmTarget;
  outputDirPath: string;
}) => {
  console.group(`Creating subpackage ${packageName}...`);

  console.log(`Preparing subpackage directory: ${outputDirPath}`);
  fs.emptyDirSync(outputDirPath);

  copyStaticFiles(TEMPLATE_STATIC_FILES, TEMPLATE_DIR, outputDirPath);

  const pkg = generatePackageJson(packageName, packageVersion, target);
  const outPkgFilePath = path.join(outputDirPath, "package.json");
  fs.writeJsonSync(outPkgFilePath, pkg);
  console.log(`Wrote package.json to: ${outPkgFilePath}`);

  console.log("Extracting tarball contents...");
  execaSync({
    // We want to see the extraction logs in our logs
    quiet: false,
    stderr: "inherit",
  })`tar -xzvf ${tarballPath} -C ${outputDirPath}`;

  console.log(`Subpackage created at: ${outputDirPath}`);

  console.groupEnd();
};

function generatePackageJson(
  packageName: string,
  packageVersion: string,
  target: NpmTarget,
) {
  console.group("Creating package.json...");

  const pkgTemplateFilePath = path.join(TEMPLATE_DIR, "package.json");
  const pkg = fs.readJsonSync(pkgTemplateFilePath) as PackageJson;
  console.log(`Read package.json template: ${pkgTemplateFilePath}`);

  pkg.name = packageName;
  pkg.version = packageVersion;
  pkg.os = [target.os];
  pkg.cpu = [target.cpu];
  if (target.libc) {
    pkg.libc = [target.libc];
  }

  // npm strips the executable bit from files in packages, so we use the "bin"
  // field to let npm know it needs to mark it as executable upon install.
  pkg.bin = {
    // The name doesn't matter (and in fact we don't want users to call it
    // directly), so we'll make it clear.
    [`__internal_wasp-${target.os}-${target.cpu}-${target.libc ?? UNDEFINED_LIBC_NAME}`]:
      "wasp-bin",
  };

  console.log("Filled out package.json fields");
  console.groupEnd();

  return pkg;
}
