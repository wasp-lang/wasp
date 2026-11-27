import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach } from "vitest";

/**
 * Gives each test of the file its own empty dir. Returns a getter for the
 * real path of the current test's dir.
 */
export function useTmpDir(): () => string {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await fs.realpath(
      await fs.mkdtemp(path.join(os.tmpdir(), "wasp-cli-test-")),
    );
  });

  afterEach(async () => {
    await fs.rm(tmpDir, { recursive: true, force: true });
  });

  return () => tmpDir;
}

export async function makeWaspProject(projectDir: string): Promise<string> {
  await fs.mkdir(projectDir, { recursive: true });
  await fs.writeFile(path.join(projectDir, ".wasproot"), "");
  return projectDir;
}

export async function makePackage(
  nodeModulesDir: string,
  name: string,
  version: string,
  packageJsonFields: object = {},
): Promise<string> {
  const packageDir = path.join(nodeModulesDir, name);
  await fs.mkdir(packageDir, { recursive: true });
  await fs.writeFile(
    path.join(packageDir, "package.json"),
    JSON.stringify({ name, version, ...packageJsonFields }),
  );
  return packageDir;
}

// A stand-in for `@wasp/cli` whose `runWaspCli` returns exit code 7. Returns
// the path of its `./internal/run-wasp-cli` export.
export async function makeWaspCliPackage(
  nodeModulesDir: string,
  version: string,
): Promise<string> {
  const packageDir = await makePackage(nodeModulesDir, "@wasp/cli", version, {
    type: "module",
    exports: {
      "./internal/run-wasp-cli": "./dist/run-wasp-cli.js",
      "./package.json": "./package.json",
    },
  });
  const runModulePath = path.join(packageDir, "dist", "run-wasp-cli.js");
  await fs.mkdir(path.dirname(runModulePath));
  await fs.writeFile(
    runModulePath,
    "export async function runWaspCli() { return 7; }\n",
  );
  return runModulePath;
}
