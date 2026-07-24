import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import path from "node:path";

type PackageJson = {
  name: string;
  version: string;
};

type ConsumerPackageJson = {
  dependencies: Record<string, string>;
};

type PackResult = {
  filename: string;
  files: { path: string }[];
};

const [packagePath, consumerPath, destinationPath] = parseArgs();
const packageDir = path.resolve(packagePath);
const consumerDir = path.resolve(consumerPath);
const destinationDir = path.resolve(consumerDir, destinationPath);
const packageJsonPath = path.join(packageDir, "package.json");
const consumerPackageJsonPath = path.join(consumerDir, "package.json");
const originalPackageJson = readFileSync(packageJsonPath, "utf8");
const packageJson = JSON.parse(originalPackageJson) as PackageJson;
const consumerPackageJson = JSON.parse(
  readFileSync(consumerPackageJsonPath, "utf8"),
) as ConsumerPackageJson;
const previousDependency = consumerPackageJson.dependencies[packageJson.name];
const contentHash = createHash("sha256");
const packedFiles = npmPack(packageDir, "--dry-run")[0].files.sort((a, b) =>
  a.path.localeCompare(b.path),
);

for (const { path: relativePath } of packedFiles) {
  contentHash.update(relativePath);
  contentHash.update("\0");
  contentHash.update(readFileSync(path.join(packageDir, relativePath)));
  contentHash.update("\0");
}

packageJson.version = `0.0.0-preview-${contentHash.digest("hex").slice(0, 12)}`;
mkdirSync(destinationDir, { recursive: true });

let tarballFilename;
try {
  writeFileSync(packageJsonPath, `${JSON.stringify(packageJson, null, 2)}\n`);
  tarballFilename = npmPack(packageDir, "--pack-destination", destinationDir)[0]
    .filename;
} finally {
  writeFileSync(packageJsonPath, originalPackageJson);
}

const tarballPath = path.join(destinationDir, tarballFilename);
const relativeTarballPath = path
  .relative(consumerDir, tarballPath)
  .split(path.sep)
  .join("/");

execFileSync(
  "npm",
  [
    "install",
    `${packageJson.name}@file:${relativeTarballPath}`,
    "--save",
    "--package-lock-only",
    "--ignore-scripts",
  ],
  { cwd: consumerDir, stdio: "inherit" },
);

if (
  previousDependency?.startsWith("file:") &&
  previousDependency.endsWith(".tgz")
) {
  const previousTarballPath = path.resolve(
    consumerDir,
    previousDependency.slice("file:".length),
  );
  if (
    path.dirname(previousTarballPath) === destinationDir &&
    previousTarballPath !== tarballPath
  ) {
    rmSync(previousTarballPath, { force: true });
  }
}

console.log(`Packed ${packageJson.name}@${packageJson.version}`);

function parseArgs(): [string, string, string] {
  const args = process.argv.slice(2);
  if (args.length !== 3) {
    console.error(
      "Usage: node scripts/pack-preview.ts <package-dir> <consumer-dir> <consumer-relative-destination>",
    );
    throw new Error("Invalid number of arguments");
  }
  return args;
}

function npmPack(workingDir: string, ...args: string[]): PackResult[] {
  return JSON.parse(
    execFileSync("npm", ["pack", "--json", ...args], {
      cwd: workingDir,
      encoding: "utf8",
    }),
  );
}
