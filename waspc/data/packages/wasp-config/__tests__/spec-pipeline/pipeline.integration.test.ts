import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { pathToFileURL } from "url";
import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { analyzeApp } from "../../src/legacy/appAnalyzer.js";
import { compileWaspTsToJs } from "../../src/spec-pipeline/compile.js";

// We use the absolute file:// URL of the local wasp-config source so the
// rewritten spec file does not rely on node_modules resolution from a temp dir.
const waspConfigEntryUrl = pathToFileURL(
  join(__dirname, "..", "..", "src", "index.ts"),
).href;

const importFormPrelude = [
  `import MainPage from "@src/MainPage";`,
  `import { getTasks } from "@src/operations";`,
  `import { archive as archiveTask } from "@src/operations";`,
  `import { archive as archiveLegacyTask } from "@src/legacyOperations";`,
  `import * as ops from "@src/operations";`,
];

const descriptorFormPrelude = [
  `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;`,
  `const getTasks = { import: "getTasks", from: "@src/operations" } as const;`,
  `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;`,
  `const archiveLegacyTask = { import: "archive", from: "@src/legacyOperations", alias: "archiveLegacyTask" } as const;`,
  `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;`,
];

const appSpecBody = [
  `const app = new App("demo", {`,
  `  title: "Demo",`,
  `  wasp: { version: "^0.16.0" },`,
  `});`,
  ``,
  `const mainPage = app.page("MainPage", { component: MainPage });`,
  `app.route("RootRoute", { path: "/", to: mainPage });`,
  `app.query("getTasks", { fn: getTasks, entities: [] });`,
  `app.action("archiveTask", { fn: archiveTask, entities: [] });`,
  `app.action("archiveLegacyTask", { fn: archiveLegacyTask, entities: [] });`,
  `app.action("logout", { fn: ops.logout, entities: [] });`,
  ``,
  `export default app;`,
];

describe("end-to-end import-form pipeline", () => {
  test("compiles import-form specs without writing rewritten TS", async () => {
    const tempDir = makeTempProject("wasp-spec-pipeline-");
    writeSourceFiles(tempDir);

    const importDecls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: specSource(importFormPrelude),
    });
    const descriptorDecls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.descriptor.ts",
      sourceText: specSource(descriptorFormPrelude),
    });

    expect(importDecls).toEqual(descriptorDecls);
    expect(importDecls).toEqual(expectedDecls());
  });

  test("rejects unsupported imports during virtual compile", () => {
    const tempDir = makeTempProject("wasp-spec-pipeline-error-");
    mkdirSync(join(tempDir, "src"));
    writeFileSync(join(tempDir, "src", "setup.ts"), `export {};\n`);

    expect(() =>
      compileSpec({
        tempDir,
        sourceFileName: "main.wasp.ts",
        sourceText: `import "@src/setup";\n`,
      }),
    ).toThrowError(/Side-effect imports/);
  });

  test("type-checks the rewritten spec shape before execution", () => {
    const tempDir = makeTempProject("wasp-spec-typecheck-");
    mkdirSync(join(tempDir, "src"));

    writeFileSync(
      join(tempDir, "src", "title.ts"),
      `export const appTitle = "Demo";\n`,
    );

    expect(() =>
      compileSpec({
        tempDir,
        sourceFileName: "main.wasp.ts",
        sourceText: [
          `import { appTitle } from "@src/title";`,
          ``,
          `declare function makeApp(config: { title: string }): void;`,
          `makeApp({ title: appTitle });`,
        ].join("\n"),
      }),
    ).toThrowError(/is not assignable to type 'string'/);
  });

  test("type-checks @src imports against real source files", () => {
    const tempDir = makeTempProject("wasp-spec-src-import-");
    mkdirSync(join(tempDir, "src"));

    writeFileSync(
      join(tempDir, "src", "title.ts"),
      `export const appTitle = "Demo";\n`,
    );

    expect(() =>
      compileSpec({
        tempDir,
        sourceFileName: "main.wasp.ts",
        sourceText: `import { typoedTitle } from "@src/title";\ntypoedTitle;\n`,
      }),
    ).toThrowError(/has no exported member 'typoedTitle'/);
  });
});

function specSource(prelude: string[]): string {
  return [
    `// @ts-ignore: This test imports the local TS source through Vitest.`,
    `import { App } from ${JSON.stringify(waspConfigEntryUrl)};`,
    ...prelude,
    ``,
    ...appSpecBody,
  ].join("\n");
}

function expectedDecls(): AppSpec.Decl[] {
  return [
    {
      declType: "App",
      declName: "demo",
      declValue: expect.objectContaining({
        title: "Demo",
        wasp: { version: "^0.16.0" },
      }),
    },
    {
      declType: "Page",
      declName: "MainPage",
      declValue: {
        component: {
          kind: "default",
          name: "MainPage",
          path: "@src/MainPage",
        },
        authRequired: undefined,
      },
    },
    {
      declType: "Route",
      declName: "RootRoute",
      declValue: {
        path: "/",
        to: { declType: "Page", name: "MainPage" },
        prerender: undefined,
        lazy: undefined,
      },
    },
    {
      declType: "Action",
      declName: "archiveTask",
      declValue: {
        fn: {
          kind: "named",
          name: "archive",
          alias: "archiveTask",
          path: "@src/operations",
        },
        entities: [],
        auth: undefined,
      },
    },
    {
      declType: "Action",
      declName: "archiveLegacyTask",
      declValue: {
        fn: {
          kind: "named",
          name: "archive",
          alias: "archiveLegacyTask",
          path: "@src/legacyOperations",
        },
        entities: [],
        auth: undefined,
      },
    },
    {
      declType: "Action",
      declName: "logout",
      declValue: {
        fn: {
          kind: "named",
          name: "logout",
          alias: "ops_logout",
          path: "@src/operations",
        },
        entities: [],
        auth: undefined,
      },
    },
    {
      declType: "Query",
      declName: "getTasks",
      declValue: {
        fn: {
          kind: "named",
          name: "getTasks",
          path: "@src/operations",
        },
        entities: [],
        auth: undefined,
      },
    },
  ] as AppSpec.Decl[];
}

function makeTempProject(prefix: string): string {
  const tempDir = mkdtempSync(join(tmpdir(), prefix));
  writePackageJson(tempDir);
  return tempDir;
}

function writePackageJson(tempDir: string): void {
  writeFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({ type: "module" }),
  );
}

function writeSourceFiles(tempDir: string): void {
  mkdirSync(join(tempDir, "src"));
  writeFileSync(
    join(tempDir, "src", "MainPage.ts"),
    `export default function MainPage() { return null; }\n`,
  );
  writeFileSync(
    join(tempDir, "src", "operations.ts"),
    [
      `export async function getTasks() { return []; }`,
      `export async function archive() { return null; }`,
      `export async function logout() { return null; }`,
      ``,
    ].join("\n"),
  );
  writeFileSync(
    join(tempDir, "src", "legacyOperations.ts"),
    `export async function archive() { return null; }\n`,
  );
}

async function compileAndAnalyzeSpec({
  tempDir,
  sourceFileName,
  sourceText,
}: {
  tempDir: string;
  sourceFileName: string;
  sourceText: string;
}): Promise<AppSpec.Decl[]> {
  const compiledPath = compileSpec({ tempDir, sourceFileName, sourceText });
  const result = await analyzeApp(pathToFileURL(compiledPath).href, []);
  if (result.status === "error") throw new Error(result.error);

  return result.value;
}

function compileSpec({
  tempDir,
  sourceFileName,
  sourceText,
}: {
  tempDir: string;
  sourceFileName: string;
  sourceText: string;
}): string {
  const sourcePath = join(tempDir, sourceFileName);
  const tsconfigPath = join(
    tempDir,
    sourceFileName.replace(/\.ts$/, ".tsconfig.json"),
  );
  const compiledPath = join(
    tempDir,
    ".wasp",
    sourceFileName.replace(/\.ts$/, ".js"),
  );

  writeFileSync(sourcePath, sourceText, "utf8");
  writeTsConfig(tsconfigPath, sourceFileName);
  compileWaspTsToJs({
    inputPath: sourcePath,
    tsconfigPath,
    outputPath: compiledPath,
  });
  expect(existsSync(sourcePath.replace(/\.ts$/, ".rewritten.ts"))).toBe(false);

  return compiledPath;
}

function writeTsConfig(tsconfigPath: string, include: string): void {
  writeFileSync(
    tsconfigPath,
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "bundler",
        strict: true,
        noEmit: true,
        baseUrl: ".",
        paths: {
          "@src/*": ["src/*"],
        },
      },
      include: [include],
    }),
  );
}
