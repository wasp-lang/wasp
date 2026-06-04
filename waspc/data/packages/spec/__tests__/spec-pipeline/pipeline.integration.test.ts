import * as cp from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { beforeAll, describe, expect, test } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";

const SPEC_PACKAGE_DIR = path.resolve(import.meta.dirname, "..", "..");

describe("Wasp TS spec pipeline", () => {
  // These tests install the package into a temp project and run its built CLI
  // (`npx @wasp.sh/spec` -> dist/src/run.js). CI runs the test suite without a
  // prior build, so build the package here to keep the tests self-contained.
  beforeAll(() => {
    cp.execSync("npm run build", { cwd: SPEC_PACKAGE_DIR, stdio: "inherit" });
  }, 120_000);

  test("analyzes split specs with lowered ref imports", () => {
    using project = makeTempProject("wasp-spec-pipeline-");

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

    const decls = project.analyzeSpec(
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

    expect(decls).toEqual({
      status: "ok",
      value: expect.arrayContaining([
        expect.objectContaining({ declType: "App", declName: "demo" }),
      ]),
    });
    expect(decls).toEqual({
      status: "ok",
      value: expect.arrayContaining([
        expect.objectContaining({ declType: "Page", declName: "MainPage" }),
      ]),
    });
    expect(decls).toEqual({
      status: "ok",
      value: expect.arrayContaining([
        expect.objectContaining({
          declType: "Action",
          declName: "adminOperations_archive",
        }),
      ]),
    });
  });

  test("surfaces type errors in the spec as a SpecUserError with formatted diagnostics", () => {
    using project = makeTempProject("wasp-spec-pipeline-type-error-");

    const result = project.analyzeSpec(
      [
        `import { app } from "@wasp.sh/spec";`,
        ``,
        `export const oops: string = 123;`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  decls: [],`,
        `});`,
      ].join("\n"),
    );

    expect(result).toEqual({
      status: "error",
      error: expect.stringContaining(
        "Type 'number' is not assignable to type 'string'",
      ),
    });
  });
});

type TempProject = Disposable & {
  writeProjectFile: (relativeFilePath: string, sourceText: string) => void;
  analyzeSpec: (sourceText: string) => ReturnType<typeof analyzeApp>;
};

function makeTempProject(prefix: string): TempProject {
  const projectRootDir = fs.mkdtempSync(path.join(os.tmpdir(), prefix));

  return scaffoldProject({
    projectRootDir,
    dispose: () => fs.rmSync(projectRootDir, { recursive: true, force: true }),
  });
}

function scaffoldProject({
  projectRootDir,
  dispose,
}: {
  projectRootDir: string;
  dispose: () => void;
}): TempProject {
  const tsconfigPath = path.join(projectRootDir, "tsconfig.json");

  fs.writeFileSync(
    path.join(projectRootDir, "package.json"),
    JSON.stringify({
      type: "module",
      dependencies: { "@wasp.sh/spec": "file:" + SPEC_PACKAGE_DIR },
    }),
  );

  fs.writeFileSync(
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
    [Symbol.dispose]: dispose,

    writeProjectFile: (relativeFilePath: string, sourceText: string) => {
      writeProjectFile(projectRootDir, relativeFilePath, sourceText);
    },

    analyzeSpec: (sourceText: string) => {
      writeProjectFile(projectRootDir, "main.wasp.ts", sourceText);

      cp.execSync("npm i", { cwd: projectRootDir, stdio: "inherit" });
      cp.execSync(
        "npx @wasp.sh/spec analyze main.wasp.ts tsconfig.json . result.json '[]'",
        { cwd: projectRootDir, stdio: "inherit" },
      );

      return JSON.parse(
        fs.readFileSync(path.join(projectRootDir, "result.json"), "utf8"),
      );
    },
  };
}

function writeProjectFile(
  projectRootDir: string,
  relativeFilePath: string,
  sourceText: string,
): void {
  const filePath = path.join(projectRootDir, relativeFilePath);
  writeFile(filePath, sourceText);
}

function writeFile(filePath: string, sourceText: string): void {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  fs.writeFileSync(filePath, sourceText, "utf8");
}
