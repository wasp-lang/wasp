import * as cp from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { describe, expect, test } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";

const SPEC_PACKAGE_DIR = path.resolve(import.meta.dirname, "..", "..");
const OWN_ANALYZER_SCRIPT = path.join(SPEC_PACKAGE_DIR, "dist/src/run.js");

describe("Wasp TS spec pipeline", () => {
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
        `import { archive } from "../adminOperations" with { type: "ref" };`,
        ``,
        `export const splitTitle = "Split Demo";`,
        `export const archiveAction = action(archive, { entities: [] });`,
      ].join("\n"),
    );

    project.writeProjectFile(
      "src/features/faq.wasp.ts",
      [
        `import { page } from "@wasp.sh/spec";`,
        `import { splitTitle } from "./tasks.wasp";`,
        `import FaqPage from "./faq/FaqPage" with { type: "ref" };`,
        ``,
        `export const faqPage = page(FaqPage);`,
      ].join("\n"),
    );
    project.writeProjectFile(
      "src/features/faq/FaqPage.ts",
      [`export defautl function FaqPage() { return null; }`].join("\n"),
    );

    const result = project.analyzeSpec(
      [
        `import { app } from "@wasp.sh/spec";`,
        `import { homePage } from "./src/features/home.wasp";`,
        `import { archiveAction, splitTitle } from "./src/features/tasks.wasp";`,
        `import { faqPage } from "./src/features/faq.wasp";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: splitTitle,`,
        `  wasp: { version: "^0.16.0" },`,
        `  spec: [homePage, archiveAction, faqPage],`,
        `});`,
      ].join("\n"),
    );

    expect(result).toMatchSnapshot();
  });

  test("surfaces type errors in the spec as a WaspSpecUserError with formatted diagnostics", () => {
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
        `  spec: [],`,
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

  test("surfaces a WaspSpecUserError thrown by the user's spec as a clean error", () => {
    using project = makeTempProject("wasp-spec-pipeline-user-error-");

    const result = project.analyzeSpec(
      [
        `import { app, WaspSpecUserError } from "@wasp.sh/spec";`,
        ``,
        `function requireEnv(name: string): string {`,
        `  throw new WaspSpecUserError(\`Missing environment variable '\${name}'.\`);`,
        `}`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: requireEnv("MY_SECRET"),`,
        `  wasp: { version: "^0.16.0" },`,
        `  spec: [],`,
        `});`,
      ].join("\n"),
    );

    expect(result).toEqual({
      status: "error",
      error: "Missing environment variable 'MY_SECRET'.",
    });
  });

  test("rejects namespace ref imports with a WaspSpecUserError", () => {
    using project = makeTempProject("wasp-spec-pipeline-namespace-");

    project.writeProjectFile(
      "src/operations.ts",
      `export async function archive() { return null; }\n`,
    );

    const result = project.analyzeSpec(
      [
        `import { app } from "@wasp.sh/spec";`,
        `import * as ops from "./src/operations" with { type: "ref" };`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  spec: [],`,
        `});`,
      ].join("\n"),
    );

    expect(result).toEqual({
      status: "error",
      error: expect.stringContaining(
        "Namespace imports are not supported for reference imports",
      ),
    });
  });

  test("analyzes a project whose dependencies aren't installed", () => {
    using project = makeTempProject("wasp-spec-pipeline-no-deps-");

    project.writeProjectFile(
      "tsconfig.json",
      JSON.stringify({
        compilerOptions: {
          target: "ES2022",
          module: "ESNext",
          moduleResolution: "bundler",
          strict: true,
          noEmit: true,
          types: ["node"],
        },
        include: ["main.wasp.ts"],
      }),
    );

    const result = project.analyzeSpecWithoutInstallingDependencies(
      [
        `import { app } from "@wasp.sh/spec";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: process.env.DEMO_TITLE ?? "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  spec: [],`,
        `});`,
      ].join("\n"),
    );

    expect(result).toEqual({ status: "ok", value: expect.any(Array) });
    expect(project.hasProjectPath("node_modules")).toBe(false);
    expect(project.hasProjectPath(".wasp/spec-bundle")).toBe(false);
  });

  test("gives userland libraries the analyzer's own @wasp.sh/spec", () => {
    using project = makeTempProject("wasp-spec-pipeline-userland-lib-");

    // A library in the project's `node_modules` that imports `@wasp.sh/spec`,
    // while the project has no `@wasp.sh/spec` of its own.
    project.writeProjectFile(
      "node_modules/spec-helpers/package.json",
      JSON.stringify({
        name: "spec-helpers",
        type: "module",
        exports: { ".": { types: "./index.d.ts", default: "./index.js" } },
      }),
    );
    project.writeProjectFile(
      "node_modules/spec-helpers/index.js",
      [
        `import { WaspSpecUserError } from "@wasp.sh/spec";`,
        `export function requirePositive(n) {`,
        `  if (n <= 0) throw new WaspSpecUserError(\`Expected a positive number, got \${n}.\`);`,
        `  return n;`,
        `}`,
      ].join("\n"),
    );
    project.writeProjectFile(
      "node_modules/spec-helpers/index.d.ts",
      `export declare function requirePositive(n: number): number;\n`,
    );

    const result = project.analyzeSpecWithoutInstallingDependencies(
      [
        `import { app } from "@wasp.sh/spec";`,
        `import { requirePositive } from "spec-helpers";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: String(requirePositive(0)),`,
        `  wasp: { version: "^0.16.0" },`,
        `  spec: [],`,
        `});`,
      ].join("\n"),
    );

    // Only a single `@wasp.sh/spec` module instance turns the library's error
    // into a clean user error.
    expect(result).toEqual({
      status: "error",
      error: "Expected a positive number, got 0.",
    });
  });
});

type TempProject = Disposable & {
  writeProjectFile: (relativeFilePath: string, sourceText: string) => void;
  hasProjectPath: (relativePath: string) => boolean;
  analyzeSpec: (sourceText: string) => ReturnType<typeof analyzeApp>;
  // Runs the analyzer from this package, the way the Wasp CLI does, without
  // installing anything into the project.
  analyzeSpecWithoutInstallingDependencies: (
    sourceText: string,
  ) => ReturnType<typeof analyzeApp>;
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

    hasProjectPath: (relativePath: string) =>
      fs.existsSync(path.join(projectRootDir, relativePath)),

    analyzeSpec: (sourceText: string) => {
      writeProjectFile(projectRootDir, "main.wasp.ts", sourceText);

      cp.execSync("npm i", { cwd: projectRootDir, stdio: "inherit" });
      cp.execSync(
        "npx @wasp.sh/spec analyze main.wasp.ts tsconfig.json . result.json '[]'",
        { cwd: projectRootDir, stdio: "inherit" },
      );

      return readAnalysisResult(projectRootDir);
    },

    analyzeSpecWithoutInstallingDependencies: (sourceText: string) => {
      writeProjectFile(projectRootDir, "main.wasp.ts", sourceText);

      cp.execFileSync(
        "node",
        [
          OWN_ANALYZER_SCRIPT,
          "analyze",
          "main.wasp.ts",
          "tsconfig.json",
          ".",
          "result.json",
          "[]",
        ],
        { cwd: projectRootDir, stdio: "inherit" },
      );

      return readAnalysisResult(projectRootDir);
    },
  };
}

function readAnalysisResult(projectRootDir: string) {
  return JSON.parse(
    fs.readFileSync(path.join(projectRootDir, "result.json"), "utf8"),
  );
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
