import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { CLIError } from "../src/cli-error.js";
import {
  findProjectWaspCliToHandOverTo,
  type InstalledWaspCli,
  runProjectWaspCli,
} from "../src/project-wasp-cli.js";

// A global Wasp CLI, installed somewhere else than the project.
const globalWaspCli: InstalledWaspCli = {
  runModulePath: "/global/node_modules/@wasp/cli/dist/run.js",
  version: "0.27.0",
};

let tmpDir: string;
let originalCwd: string;

beforeEach(async () => {
  originalCwd = process.cwd();
  tmpDir = await fs.realpath(
    await fs.mkdtemp(path.join(os.tmpdir(), "wasp-cli-test-")),
  );
});

afterEach(async () => {
  process.chdir(originalCwd);
  await fs.rm(tmpDir, { recursive: true, force: true });
});

describe("findProjectWaspCliToHandOverTo", () => {
  test("runs itself outside of a Wasp project", async () => {
    process.chdir(tmpDir);

    expect(await findProjectWaspCliToHandOverTo(globalWaspCli)).toBeUndefined();
  });

  test("runs itself when the project has no @wasp/wasp installed", async () => {
    const projectDir = await makeWaspProject("app");
    process.chdir(projectDir);

    expect(await findProjectWaspCliToHandOverTo(globalWaspCli)).toBeUndefined();
  });

  test("finds the project's CLI in an npm layout", async () => {
    const projectDir = await makeWaspProject("app");
    const nodeModulesDir = path.join(projectDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");
    process.chdir(projectDir);

    expect(await findProjectWaspCliToHandOverTo(globalWaspCli)).toEqual({
      runModulePath,
      version: "0.28.0",
    });
  });

  test("finds the project's CLI from a subdirectory of the project", async () => {
    const projectDir = await makeWaspProject("app");
    const nodeModulesDir = path.join(projectDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");
    const srcDir = path.join(projectDir, "src");
    await fs.mkdir(srcDir);
    process.chdir(srcDir);

    expect(
      (await findProjectWaspCliToHandOverTo(globalWaspCli))?.runModulePath,
    ).toBe(runModulePath);
  });

  test("finds packages that a monorepo hoisted to a parent dir", async () => {
    const projectDir = await makeWaspProject(path.join("apps", "app"));
    const nodeModulesDir = path.join(tmpDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");
    process.chdir(projectDir);

    expect(
      (await findProjectWaspCliToHandOverTo(globalWaspCli))?.runModulePath,
    ).toBe(runModulePath);
  });

  test("finds the project's CLI in a pnpm layout", async () => {
    // pnpm links `@wasp/wasp` into the project from its virtual store, and
    // links `@wasp/cli` only next to the real `@wasp/wasp`.
    const projectDir = await makeWaspProject("app");
    const storeNodeModulesDir = path.join(
      projectDir,
      "node_modules",
      ".pnpm",
      "@wasp+wasp@0.28.0",
      "node_modules",
    );
    const waspPackageDir = await makePackage(
      storeNodeModulesDir,
      "@wasp/wasp",
      "0.28.0",
    );
    const runModulePath = await makeWaspCliPackage(
      storeNodeModulesDir,
      "0.28.0",
    );
    await fs.mkdir(path.join(projectDir, "node_modules", "@wasp"));
    await fs.symlink(
      waspPackageDir,
      path.join(projectDir, "node_modules", "@wasp", "wasp"),
    );
    process.chdir(projectDir);

    expect(
      (await findProjectWaspCliToHandOverTo(globalWaspCli))?.runModulePath,
    ).toBe(runModulePath);
  });

  test("runs itself when it is the project's CLI", async () => {
    const projectDir = await makeWaspProject("app");
    const nodeModulesDir = path.join(projectDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");
    process.chdir(projectDir);

    expect(
      await findProjectWaspCliToHandOverTo({
        runModulePath,
        version: "0.28.0",
      }),
    ).toBeUndefined();
  });

  test("fails when the project's @wasp/wasp has no @wasp/cli", async () => {
    const projectDir = await makeWaspProject("app");
    await makePackage(
      path.join(projectDir, "node_modules"),
      "@wasp/wasp",
      "0.28.0",
    );
    process.chdir(projectDir);

    await expect(findProjectWaspCliToHandOverTo(globalWaspCli)).rejects.toThrow(
      CLIError,
    );
  });
});

describe("runProjectWaspCli", () => {
  test("calls the project CLI's runWaspCli and returns its exit code", async () => {
    const runModulePath = await makeWaspCliPackage(
      path.join(tmpDir, "node_modules"),
      "0.27.0",
    );

    expect(
      await runProjectWaspCli(
        { runModulePath, version: "0.27.0" },
        globalWaspCli,
      ),
    ).toBe(7);
  });

  test("says which version runs when it differs from the called CLI", async () => {
    const consoleError = vi
      .spyOn(console, "error")
      .mockImplementation(() => {});
    const runModulePath = await makeWaspCliPackage(
      path.join(tmpDir, "node_modules"),
      "0.28.0",
    );

    await runProjectWaspCli(
      { runModulePath, version: "0.28.0" },
      globalWaspCli,
    );

    expect(consoleError).toHaveBeenCalledWith(
      "Running Wasp 0.28.0 from this project. The Wasp CLI you called is 0.27.0.",
    );
    consoleError.mockRestore();
  });
});

async function makeWaspProject(relativeDir: string): Promise<string> {
  const projectDir = path.join(tmpDir, relativeDir);
  await fs.mkdir(projectDir, { recursive: true });
  await fs.writeFile(path.join(projectDir, ".wasproot"), "");
  return projectDir;
}

async function makePackage(
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
// the path of its `./run` export.
async function makeWaspCliPackage(
  nodeModulesDir: string,
  version: string,
): Promise<string> {
  const packageDir = await makePackage(nodeModulesDir, "@wasp/cli", version, {
    type: "module",
    exports: {
      "./run": "./dist/run.js",
      "./package.json": "./package.json",
    },
  });
  const runModulePath = path.join(packageDir, "dist", "run.js");
  await fs.mkdir(path.dirname(runModulePath));
  await fs.writeFile(
    runModulePath,
    "export async function runWaspCli() { return 7; }\n",
  );
  return runModulePath;
}
