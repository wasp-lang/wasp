import * as fs from "node:fs/promises";
import * as path from "node:path";
import { describe, expect, test } from "vitest";
import { CLIError } from "../src/cli-error.js";
import { findProjectWaspCli } from "../src/find-project-wasp-cli.js";
import {
  makePackage,
  makeWaspCliPackage,
  makeWaspProject,
  useTmpDir,
} from "./fixtures.js";

const getTmpDir = useTmpDir();

describe("findProjectWaspCli", () => {
  test("finds nothing outside of a Wasp project", async () => {
    expect(await findProjectWaspCli(getTmpDir())).toBeUndefined();
  });

  test("finds nothing when the project has no @wasp/wasp installed", async () => {
    const projectDir = await makeWaspProject(path.join(getTmpDir(), "app"));

    expect(await findProjectWaspCli(projectDir)).toBeUndefined();
  });

  test("finds the project's CLI in an npm layout", async () => {
    const projectDir = await makeWaspProject(path.join(getTmpDir(), "app"));
    const nodeModulesDir = path.join(projectDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");

    expect(await findProjectWaspCli(projectDir)).toEqual({
      runModulePath,
      version: "0.28.0",
    });
  });

  test("finds the project's CLI from a subdirectory of the project", async () => {
    const projectDir = await makeWaspProject(path.join(getTmpDir(), "app"));
    const nodeModulesDir = path.join(projectDir, "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");
    const srcDir = path.join(projectDir, "src");
    await fs.mkdir(srcDir);

    expect((await findProjectWaspCli(srcDir))?.runModulePath).toBe(
      runModulePath,
    );
  });

  test("finds packages that a monorepo hoisted to a parent dir", async () => {
    const projectDir = await makeWaspProject(
      path.join(getTmpDir(), "apps", "app"),
    );
    const nodeModulesDir = path.join(getTmpDir(), "node_modules");
    await makePackage(nodeModulesDir, "@wasp/wasp", "0.28.0");
    const runModulePath = await makeWaspCliPackage(nodeModulesDir, "0.28.0");

    expect((await findProjectWaspCli(projectDir))?.runModulePath).toBe(
      runModulePath,
    );
  });

  test("finds the project's CLI in a pnpm layout", async () => {
    // pnpm links `@wasp/wasp` into the project from its virtual store, and
    // links `@wasp/cli` only next to the real `@wasp/wasp`.
    const projectDir = await makeWaspProject(path.join(getTmpDir(), "app"));
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

    expect((await findProjectWaspCli(projectDir))?.runModulePath).toBe(
      runModulePath,
    );
  });

  test("fails when the project's @wasp/wasp has no @wasp/cli", async () => {
    const projectDir = await makeWaspProject(path.join(getTmpDir(), "app"));
    await makePackage(
      path.join(projectDir, "node_modules"),
      "@wasp/wasp",
      "0.28.0",
    );

    await expect(findProjectWaspCli(projectDir)).rejects.toThrow(CLIError);
  });
});
