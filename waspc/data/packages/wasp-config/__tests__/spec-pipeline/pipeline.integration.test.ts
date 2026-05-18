import { mkdirSync, mkdtempSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { dirname, join } from "path";
import { pathToFileURL } from "url";
import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";

// We use the absolute file:// URL of the local wasp-config source so the
// loaded spec does not rely on node_modules resolution from a temp dir.
const waspConfigEntryUrl = pathToFileURL(
  join(__dirname, "..", "..", "src", "spec", "publicApi", "index.ts"),
).href;

const importFormPrelude = [
  `import MainPage from "@src/MainPage";`,
  `import { getTasks } from "@src/operations";`,
  `import { archive as archiveTask } from "@src/operations";`,
  `import { archive as archiveAdminTask } from "@src/adminOperations";`,
  `import * as ops from "@src/operations";`,
];

const extImportFormPrelude = [
  `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;`,
  `const getTasks = { import: "getTasks", from: "@src/operations" } as const;`,
  `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;`,
  `const archiveAdminTask = { import: "archive", from: "@src/adminOperations", alias: "archiveAdminTask" } as const;`,
  `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;`,
];

const appSpecBody = [
  `const demoApp = app({`,
  `  name: "demo",`,
  `  title: "Demo",`,
  `  wasp: { version: "^0.16.0" },`,
  `  parts: [`,
  `    page(MainPage),`,
  `    query(getTasks, { entities: [] }),`,
  `    action(archiveTask, { entities: [] }),`,
  `    action(archiveAdminTask, { entities: [] }),`,
  `    action(ops.logout, { entities: [] }),`,
  `  ],`,
  `});`,
  ``,
  `export default demoApp;`,
];

describe("end-to-end import-form pipeline", () => {
  test("loads import-form specs through virtual source", async () => {
    const tempDir = makeTempProject("wasp-spec-pipeline-");
    writeSourceFiles(tempDir);

    const importDecls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: specSource(importFormPrelude),
    });
    const extImportDecls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ext-import.ts",
      sourceText: specSource(extImportFormPrelude),
    });

    expect(importDecls).toEqual(extImportDecls);
    expect(importDecls).toEqual(expectedDecls());
  });

  test("rejects unsupported imports during virtual loading", async () => {
    const tempDir = makeTempProject("wasp-spec-pipeline-error-");
    mkdirSync(join(tempDir, "src"));
    writeFileSync(join(tempDir, "src", "setup.ts"), `export {};\n`);

    await expect(
      compileAndAnalyzeSpec({
        tempDir,
        sourceFileName: "main.wasp.ts",
        sourceText: `import "@src/setup";\n`,
      }),
    ).rejects.toThrowError(/Side-effect imports/);
  });

  test("lowers @src imports without executing source files before generation", async () => {
    const tempDir = makeTempProject("wasp-spec-src-import-");
    mkdirSync(join(tempDir, "src"));

    writeFileSync(
      join(tempDir, "src", "operations.ts"),
      [
        `import { HttpError } from "wasp/server";`,
        `export async function getTasks() { throw new HttpError(500); }`,
        ``,
      ].join("\n"),
    );

    await expect(
      compileAndAnalyzeSpec({
        tempDir,
        sourceFileName: "main.wasp.ts",
        sourceText: [
          `// @ts-ignore: This test imports the local TS source through Vitest.`,
          `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
          `import { getTasks } from "@src/operations";`,
          `getTasks;`,
          ``,
          `export default app({`,
          `  name: "demo",`,
          `  title: "Demo",`,
          `  wasp: { version: "^0.16.0" },`,
          `  parts: [],`,
          `});`,
        ].join("\n"),
      }),
    ).resolves.toContainEqual(expect.objectContaining({ declType: "App" }));
  });

  test("executes extensionless relative TS imports from the project root", async () => {
    const tempDir = makeTempProject("wasp-spec-extensionless-import-");

    writeFileSync(
      join(tempDir, "appConfig.ts"),
      `export const appTitle = "Demo from helper";\n`,
    );

    const decls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
        `import { appTitle } from "./appConfig";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: appTitle,`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [],`,
        `});`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(
      expect.objectContaining({
        declType: "App",
        declValue: expect.objectContaining({ title: "Demo from helper" }),
      }),
    );
  });

  test("loads app from async default export", async () => {
    const tempDir = makeTempProject("wasp-spec-async-default-export-");

    const decls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
        ``,
        `export default Promise.resolve(app({`,
        `  name: "demo",`,
        `  title: "Async Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [],`,
        `}));`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(
      expect.objectContaining({
        declType: "App",
        declValue: expect.objectContaining({ title: "Async Demo" }),
      }),
    );
  });

  test("loads split .wasp.ts specs and lowers nested @src imports", async () => {
    const tempDir = makeTempProject("wasp-spec-split-");
    writeSourceFiles(tempDir);
    writeNodePackage(tempDir, "spec-helper-package", {
      js: `export const packageTitle = "Split Demo";\n`,
      dts: `export declare const packageTitle: string;\n`,
    });

    writeProjectFile(
      tempDir,
      "src/features/home.wasp.ts",
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { page } from ${JSON.stringify(waspConfigEntryUrl)};`,
        `import MainPage from "@src/MainPage";`,
        ``,
        `export const homePage = page(MainPage);`,
      ].join("\n"),
    );
    writeProjectFile(
      tempDir,
      "features/tasks.wasp.ts",
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { action } from ${JSON.stringify(waspConfigEntryUrl)};`,
        `import { packageTitle } from "spec-helper-package";`,
        `import { archive as archiveTask } from "@src/adminOperations";`,
        ``,
        `export const splitTitle = packageTitle;`,
        `export const archiveAction = action(archiveTask, { entities: [] });`,
      ].join("\n"),
    );

    const decls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
        `import { homePage } from "./src/features/home.wasp.js";`,
        `import { archiveAction, splitTitle } from "./features/tasks.wasp.js";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: splitTitle,`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [homePage, archiveAction],`,
        `});`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(
      expect.objectContaining({
        declType: "App",
        declValue: expect.objectContaining({ title: "Split Demo" }),
      }),
    );
    expect(decls).toContainEqual(expect.objectContaining({ declType: "Page" }));
    expect(decls).toContainEqual(
      expect.objectContaining({ declType: "Action", declName: "archiveTask" }),
    );
  });

  test("ignores unimported .wasp.ts files", async () => {
    const tempDir = makeTempProject("wasp-spec-ignored-");
    writeProjectFile(
      tempDir,
      "src/ignored.wasp.ts",
      `import "@src/setup";\nthrow new Error("should not load");\n`,
    );

    const decls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [],`,
        `});`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(expect.objectContaining({ declType: "App" }));
  });

  test("allows npm imports and relative re-exports in plain helpers", async () => {
    const tempDir = makeTempProject("wasp-spec-helper-imports-");
    writeNodePackage(tempDir, "spec-helper-package", {
      js: `export const packageTitle = "Helper Demo";\n`,
      dts: `export declare const packageTitle: string;\n`,
    });
    writeProjectFile(
      tempDir,
      "helpers/title.ts",
      [
        `import { packageTitle } from "spec-helper-package";`,
        `export const title = packageTitle;`,
      ].join("\n"),
    );
    writeProjectFile(
      tempDir,
      "helpers/index.ts",
      `export { title } from "./title.js";\n`,
    );

    const decls = await compileAndAnalyzeSpec({
      tempDir,
      sourceFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
        `import { title } from "./helpers/index.js";`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title,`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [],`,
        `});`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(
      expect.objectContaining({
        declType: "App",
        declValue: expect.objectContaining({ title: "Helper Demo" }),
      }),
    );
  });

  test("rejects @src imports and re-exports in plain helpers", async () => {
    const helperCases = [
      `import MainPage from "@src/MainPage";\nMainPage;\nexport const title = "Demo";\n`,
      `import type { Props } from "@src/MainPage";\nexport const title = "Demo";\n`,
      `export { MainPage } from "@src/MainPage";\nexport const title = "Demo";\n`,
    ];

    for (const helperSource of helperCases) {
      const tempDir = makeTempProject("wasp-spec-helper-src-import-");
      writeProjectFile(tempDir, "helpers/title.ts", helperSource);

      await expect(
        compileAndAnalyzeSpec({
          tempDir,
          sourceFileName: "main.wasp.ts",
          sourceText: [
            `// @ts-ignore: This test imports the local TS source through Vitest.`,
            `import { app } from ${JSON.stringify(waspConfigEntryUrl)};`,
            `import { title } from "./helpers/title.js";`,
            ``,
            `export default app({`,
            `  name: "demo",`,
            `  title,`,
            `  wasp: { version: "^0.16.0" },`,
            `  parts: [],`,
            `});`,
          ].join("\n"),
        }),
      ).rejects.toThrowError(/plain TypeScript helper/);
    }
  });
});

function specSource(prelude: string[]): string {
  return [
    `// @ts-ignore: This test imports the local TS source through Vitest.`,
    `import { action, app, page, query } from ${JSON.stringify(
      waspConfigEntryUrl,
    )};`,
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
      declName: "archiveAdminTask",
      declValue: {
        fn: {
          kind: "named",
          name: "archive",
          alias: "archiveAdminTask",
          path: "@src/adminOperations",
        },
        entities: [],
        auth: undefined,
      },
    },
    {
      declType: "Action",
      declName: "ops_logout",
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

function writeNodePackage(
  tempDir: string,
  packageName: string,
  files: { js: string; dts: string },
): void {
  const packageDir = join(tempDir, "node_modules", packageName);
  mkdirSync(packageDir, { recursive: true });
  writeFileSync(
    join(packageDir, "package.json"),
    JSON.stringify({
      type: "module",
      exports: "./index.js",
      types: "./index.d.ts",
    }),
  );
  writeFileSync(join(packageDir, "index.js"), files.js);
  writeFileSync(join(packageDir, "index.d.ts"), files.dts);
}

function writeProjectFile(
  tempDir: string,
  relativePath: string,
  sourceText: string,
): void {
  const filePath = join(tempDir, relativePath);
  mkdirSync(dirname(filePath), { recursive: true });
  writeFileSync(filePath, sourceText, "utf8");
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
    join(tempDir, "src", "adminOperations.ts"),
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
  const sourcePath = join(tempDir, sourceFileName);
  const tsconfigPath = join(
    tempDir,
    sourceFileName.replace(/\.ts$/, ".tsconfig.json"),
  );

  writeFileSync(sourcePath, sourceText, "utf8");
  writeTsConfig(tsconfigPath, sourceFileName);

  const result = await analyzeApp({
    waspTsSpecPath: sourcePath,
    tsconfigPath,
    entityNames: [],
  });

  if (result.status === "error") throw new Error(result.error);

  return result.value;
}

function writeTsConfig(tsconfigPath: string, include: string): void {
  writeFileSync(
    tsconfigPath,
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "bundler",
        jsx: "preserve",
        strict: true,
        noEmit: true,
        baseUrl: ".",
        paths: {
          "@src/*": ["src/*"],
        },
      },
      include: [include, "**/*.wasp.ts"],
    }),
  );
}
