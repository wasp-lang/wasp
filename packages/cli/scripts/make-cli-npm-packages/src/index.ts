import slugify from "@sindresorhus/slugify";
import fs from "fs-extra";
import * as path from "node:path";
import { parseArgs } from "node:util";
import { makeCliPackage } from "./generator/cli-package.ts";
import { makeSubPackage } from "./generator/sub-package.ts";
import { ARGS_OPTIONS, ArgsSchema } from "./schema/args.ts";
import { BuildDataSchema } from "./schema/input-data.ts";

const args = parseArgs({ strict: true, options: ARGS_OPTIONS });

if (args.values.help) {
  console.log(`Available options: ${JSON.stringify(ARGS_OPTIONS, null, 2)}`);
} else {
  console.log(
    `Running program with options: ${JSON.stringify(args.values, null, 2)}`,
  );

  const {
    "input-dir": inputDir,
    "output-dir": outputDir,
    "cli-package-name": cliPackageName,
    "sub-package-name": makeSubPackageName,
  } = ArgsSchema.parse(args.values);

  const dataFilePath = path.join(inputDir, "data.json");
  console.log(`Reading input data from: ${dataFilePath}`);
  const dataFile = fs.readJsonSync(dataFilePath);
  const data = BuildDataSchema(inputDir).parse(dataFile);

  console.group("Creating subpackages in:", outputDir);
  const createdSubPackages = data.tarballs.map((tarballData) => {
    const { fileName: inputTarballPath, target } = tarballData;

    console.group("Creating subpackage");
    console.log(`Input tarball path: ${inputTarballPath}`);
    console.log(`Target: ${JSON.stringify(target, null, 2)}`);

    const subPackageName = makeSubPackageName(target);
    const subPackageOutputDir = path.join(outputDir, slugify(subPackageName));

    console.log(`Subpackage name: ${subPackageName}`);
    console.log(`Subpackage output dir: ${subPackageOutputDir}`);

    makeSubPackage({
      packageName: subPackageName,
      packageVersion: data.version,
      tarballPath: inputTarballPath,
      target,
      outputDirPath: subPackageOutputDir,
    });

    console.log("Done");
    console.groupEnd();

    return {
      packageName: subPackageName,
      outputDir: subPackageOutputDir,
      data: tarballData,
    };
  });
  console.groupEnd();

  console.group("Creating CLI package in:", outputDir);

  const cliPackageOutputDir = path.join(outputDir, slugify(cliPackageName));

  console.log(`CLI package name: ${cliPackageName}`);
  console.log(`CLI package output dir: ${cliPackageOutputDir}`);

  makeCliPackage({
    packageName: cliPackageName,
    packageVersion: data.version,
    subPackages: createdSubPackages,
    outputDirPath: cliPackageOutputDir,
  });

  console.groupEnd();
}
