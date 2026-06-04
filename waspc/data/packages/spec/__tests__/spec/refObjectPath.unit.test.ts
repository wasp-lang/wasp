import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, test } from "vitest";
import { normalizeRefObjectPath } from "../../src/spec/refObjectPath.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("normalizeRefObjectPath", () => {
  test("maps a top-level spec ref import targeting src", () => {
    using project = makeTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "./src/MainPage",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "MainPage"));
  });

  test("maps a nested spec ref import from its importing file", () => {
    using project = makeTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "./MainPage",
        importingFilePath: path.join(
          project.rootDir,
          "src",
          "features",
          "home.wasp.ts",
        ),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "features", "MainPage"));
  });

  test("maps a nested ref import that climbs to the src root", () => {
    using project = makeTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "../../MainPage",
        importingFilePath: path.join(
          project.rootDir,
          "src",
          "features",
          "home",
          "home.wasp.ts",
        ),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "MainPage"));
  });

  test("normalizes ref import path segments", () => {
    using project = makeTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "./src/features/../MainPage",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "MainPage"));
  });

  test("rejects ref imports that resolve outside src", () => {
    using project = makeTempProject();

    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toThrow(SpecUserError);
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toThrow(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects ref imports that resolve to src itself", () => {
    using project = makeTempProject();

    expect(() =>
      normalizeRefObjectPath({
        importPath: "./src",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toThrow(SpecUserError);
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./src",
        importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
        projectRootDir: project.rootDir,
      }),
    ).toThrow(/must resolve to a file inside the app src\/ directory/);
  });

  test("allows ref imports that leave src and resolve back inside", () => {
    using project = makeTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "../../../src/MainPage",
        importingFilePath: path.join(
          project.rootDir,
          "src",
          "features",
          "home",
          "home.wasp.ts",
        ),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "MainPage"));
  });

  test("handles canonical import paths when the project root is a symlink", () => {
    using project = makeSymlinkTempProject();

    expect(
      normalizeRefObjectPath({
        importPath: "./MainPage",
        importingFilePath: path.join(
          project.realRootDir,
          "src",
          "features",
          "home.wasp.ts",
        ),
        projectRootDir: project.rootDir,
      }),
    ).toBe(path.join("@src", "features", "MainPage"));
  });
});

type TempProject = Disposable & {
  rootDir: string;
};

function makeTempProject(): TempProject {
  const rootDir = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-ref-path-"));
  fs.mkdirSync(path.join(rootDir, "src"));

  return {
    rootDir,
    [Symbol.dispose]: () =>
      fs.rmSync(rootDir, { recursive: true, force: true }),
  };
}

type SymlinkTempProject = TempProject & {
  realRootDir: string;
};

function makeSymlinkTempProject(): SymlinkTempProject {
  const tempRootDir = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-ref-path-symlink-"),
  );
  const realRootDir = fs.realpathSync(tempRootDir);
  const rootDir = `${tempRootDir}-link`;

  fs.mkdirSync(path.join(realRootDir, "src"));
  fs.symlinkSync(tempRootDir, rootDir, "dir");

  return {
    rootDir,
    realRootDir,
    [Symbol.dispose]: () => {
      fs.rmSync(rootDir, { force: true });
      fs.rmSync(tempRootDir, { recursive: true, force: true });
    },
  };
}
