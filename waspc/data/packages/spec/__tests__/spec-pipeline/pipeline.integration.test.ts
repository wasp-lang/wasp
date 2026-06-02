import { mkdirSync, mkdtempSync, realpathSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { dirname, join } from "path";
import { describe, expect, test } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";

describe("Wasp TS spec pipeline", () => {
  test("analyzes split specs with lowered ref imports", async () => {
    const project = makeTempProject("wasp-spec-pipeline-");
    project.writeProjectFile(
      "src/MainPage.ts",
      `export default function MainPage() { return null; }\n`,
    );
    project.writeProjectFile(
      "src/adminOperations.ts",
      [
        `throw new Error("ref import was executed");`,
        `export async function archive() { return null; }`,
        ``,
      ].join("\n"),
    );
    project.writeProjectFile(
      "src/features/home.wasp.ts",
      [
        `import { page } from "@wasp.sh/spec";`,
        `import MainPage from "../MainPage" with { type: "ref" };`,
        ``,
        `export const homePage = page(MainPage);`,
      ].join("\n"),
    );
    project.writeProjectFile(
      "src/features/tasks.wasp.ts",
      [
        `import { action } from "@wasp.sh/spec";`,
        `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
        ``,
        `export const splitTitle = "Split Demo";`,
        `export const archiveAction = action(adminOperations.archive, { entities: [] });`,
      ].join("\n"),
    );

    const decls = await project.analyzeSpec(
      [
        `import { app } from "@wasp.sh/spec";`,
        `import { homePage } from "./src/features/home.wasp.js";`,
        `import { archiveAction, splitTitle } from "./src/features/tasks.wasp.js";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: splitTitle,`,
        `  wasp: { version: "^0.16.0" },`,
        `  decls: [homePage, archiveAction],`,
        `});`,
      ].join("\n"),
    );

    expect(decls).toContainEqual(
      expect.objectContaining({ declType: "App", declName: "demo" }),
    );
    expect(decls).toContainEqual(
      expect.objectContaining({ declType: "Page", declName: "MainPage" }),
    );
    expect(decls).toContainEqual(
      expect.objectContaining({
        declType: "Action",
        declName: "adminOperations_archive",
      }),
    );
  });
});

function makeTempProject(prefix: string): {
  writeProjectFile: (relativeFilePath: string, sourceText: string) => void;
  analyzeSpec: (sourceText: string) => ReturnType<typeof analyzeApp>;
} {
  // Jiti reports loaded files using canonical paths, so use a canonical temp root too.
  const projectRootDir = mkdtempSync(join(realpathSync(tmpdir()), prefix));
  const tsconfigPath = join(projectRootDir, "tsconfig.json");

  writeFileSync(
    join(projectRootDir, "package.json"),
    JSON.stringify({ type: "module" }),
  );
  writeSpecPackageStub(projectRootDir);
  writeFileSync(
    tsconfigPath,
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "bundler",
        jsx: "preserve",
        strict: true,
        allowJs: true,
        noEmit: true,
      },
      include: ["main.wasp.ts", "**/*.wasp.ts"],
    }),
  );
  return {
    writeProjectFile: (relativeFilePath: string, sourceText: string) => {
      const filePath = join(projectRootDir, relativeFilePath);
      mkdirSync(dirname(filePath), { recursive: true });
      writeFileSync(filePath, sourceText, "utf8");
    },
    analyzeSpec: async (sourceText: string) => {
      const sourcePath = join(projectRootDir, "main.wasp.ts");

      writeFileSync(sourcePath, sourceText, "utf8");

      return analyzeApp({
        waspTsSpecPath: sourcePath,
        tsconfigPath,
        projectRootDir,
        entityNames: [],
      });
    },
  };
}

function writeSpecPackageStub(projectRootDir: string): void {
  writeProjectFile(
    projectRootDir,
    "node_modules/@wasp.sh/spec/package.json",
    JSON.stringify({
      type: "module",
      exports: {
        ".": {
          types: "./index.d.ts",
          default: "./index.js",
        },
        "./internal": {
          types: "./internal.d.ts",
          default: "./internal.js",
        },
      },
    }),
  );
  writeProjectFile(
    projectRootDir,
    "node_modules/@wasp.sh/spec/index.js",
    [
      `export function app(config) { return config; }`,
      `export function page(component, options = {}) { return { kind: "page", component, ...options }; }`,
      `export function action(fn, options = {}) { return { kind: "action", fn, ...options }; }`,
      ``,
    ].join("\n"),
  );
  writeProjectFile(
    projectRootDir,
    "node_modules/@wasp.sh/spec/internal.js",
    [
      `export function _waspMakeRef(importingFileUrl) {`,
      `  const sourceFilePath = new URL(importingFileUrl).pathname;`,
      `  return (descriptor) => ({ ...descriptor, kind: "refObject", sourceFilePath });`,
      `}`,
      ``,
    ].join("\n"),
  );
  writeProjectFile(
    projectRootDir,
    "node_modules/@wasp.sh/spec/index.d.ts",
    [
      `export declare function app(config: any): any;`,
      `export declare function page(component: any, options?: any): any;`,
      `export declare function action(fn: any, options?: any): any;`,
      ``,
    ].join("\n"),
  );
  writeProjectFile(
    projectRootDir,
    "node_modules/@wasp.sh/spec/internal.d.ts",
    [
      `export declare function _waspMakeRef(importingFileUrl: string): (descriptor: any) => any;`,
      ``,
    ].join("\n"),
  );
}

function writeProjectFile(
  projectRootDir: string,
  relativeFilePath: string,
  sourceText: string,
): void {
  const filePath = join(projectRootDir, relativeFilePath);
  mkdirSync(dirname(filePath), { recursive: true });
  writeFileSync(filePath, sourceText, "utf8");
}
