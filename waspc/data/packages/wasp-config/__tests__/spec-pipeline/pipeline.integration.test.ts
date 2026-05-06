import { execFileSync } from "child_process";
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from "fs";
import { createRequire } from "module";
import { tmpdir } from "os";
import { join } from "path";
import { pathToFileURL } from "url";
import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { main as runWaspConfig } from "../../src/run.js";
import { rewrite } from "../../src/spec-pipeline/rewrite.js";

// We use the absolute file:// URL of the local wasp-config source so the
// rewritten spec file does not rely on node_modules resolution from a temp dir.
const waspConfigEntryUrl = pathToFileURL(
  join(__dirname, "..", "..", "src", "index.ts"),
).href;

const require = createRequire(import.meta.url);
const tscPath = require.resolve("typescript/lib/tsc.js");

const specSource = () =>
  [
    `// @ts-ignore: This test imports the local TS source through Vitest.`,
    `import { App } from ${JSON.stringify(waspConfigEntryUrl)};`,
    `import MainPage from "./src/MainPage";`,
    `import { getTasks } from "./src/operations";`,
    `import { archive as archiveTask } from "./src/operations";`,
    `import { archive as archiveLegacyTask } from "./src/legacyOperations";`,
    `import * as ops from "./src/operations";`,
    ``,
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
  ].join("\n");

const descriptorSpecSource = () =>
  [
    `// @ts-ignore: This test imports the local TS source through Vitest.`,
    `import { App } from ${JSON.stringify(waspConfigEntryUrl)};`,
    `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;`,
    `const getTasks = { import: "getTasks", from: "@src/operations" } as const;`,
    `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;`,
    `const archiveLegacyTask = { import: "archive", from: "@src/legacyOperations", alias: "archiveLegacyTask" } as const;`,
    ``,
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
    `app.action("logout", {`,
    `  fn: { import: "logout", from: "@src/operations", alias: "ops_logout" } as const,`,
    `  entities: [],`,
    `});`,
    ``,
    `export default app;`,
  ].join("\n");

describe("end-to-end import-form pipeline", () => {
  test("runs rewrite, type-check, and execute commands for every import shape", async () => {
    const tempDir = mkdtempSync(join(tmpdir(), "wasp-spec-pipeline-"));
    writePackageJson(tempDir);

    const importDecls = await runSpecThroughRunTs(
      tempDir,
      "main.wasp.ts",
      specSource(),
      "import-decls.json",
    );
    const descriptorDecls = await runSpecThroughRunTs(
      tempDir,
      "main.wasp.descriptor.ts",
      descriptorSpecSource(),
      "descriptor-decls.json",
    );

    expect(importDecls).toEqual(descriptorDecls);

    const decls = importDecls;
    const declsByType = groupByType(decls);

    expect(declsByType["Page"]).toEqual([
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
    ]);

    const queryDecl = declsByType["Query"]?.[0];
    expect(queryDecl?.declName).toBe("getTasks");
    expect(queryDecl?.declValue.fn).toEqual({
      kind: "named",
      name: "getTasks",
      path: "@src/operations",
    });

    const actionDecls = declsByType["Action"] ?? [];
    const archiveAction = actionDecls.find((d) => d.declName === "archiveTask");
    expect(archiveAction?.declValue.fn).toEqual({
      kind: "named",
      name: "archive",
      alias: "archiveTask",
      path: "@src/operations",
    });

    const archiveLegacyAction = actionDecls.find(
      (d) => d.declName === "archiveLegacyTask",
    );
    expect(archiveLegacyAction?.declValue.fn).toEqual({
      kind: "named",
      name: "archive",
      alias: "archiveLegacyTask",
      path: "@src/legacyOperations",
    });

    const logoutAction = actionDecls.find((d) => d.declName === "logout");
    expect(logoutAction?.declValue.fn).toEqual({
      kind: "named",
      name: "logout",
      alias: "ops_logout",
      path: "@src/operations",
    });
  });

  test("rejects unsupported imports through the run.ts rewrite command", async () => {
    const tempDir = mkdtempSync(join(tmpdir(), "wasp-spec-pipeline-error-"));
    const sourcePath = join(tempDir, "main.wasp.ts");
    const rewrittenPath = join(tempDir, "main.wasp.rewritten.ts");

    writeFileSync(sourcePath, `import helper from "./helpers";\n`, "utf8");

    await expect(
      runWaspConfig(["node", "run.js", "rewrite", sourcePath, rewrittenPath]),
    ).rejects.toThrowError(/Relative imports outside \.\/src/);
  });

  test("type-checks the rewritten spec shape before execution", () => {
    const tempDir = mkdtempSync(join(tmpdir(), "wasp-spec-typecheck-"));
    mkdirSync(join(tempDir, "src"));

    const source = [
      `import { appTitle } from "./src/title";`,
      ``,
      `declare function makeApp(config: { title: string }): void;`,
      `makeApp({ title: appTitle });`,
    ].join("\n");

    writeFileSync(
      join(tempDir, "src", "title.ts"),
      `export const appTitle = "Demo";\n`,
    );
    writeFileSync(join(tempDir, "main.wasp.ts"), source);
    writeTsConfig(tempDir, "tsconfig.original.json", "main.wasp.ts");
    runTsc(tempDir, "tsconfig.original.json");

    writeFileSync(join(tempDir, "main.wasp.rewritten.ts"), rewrite(source));
    writeTsConfig(tempDir, "tsconfig.rewritten.json", "main.wasp.rewritten.ts");

    expect(runTscExpectingError(tempDir, "tsconfig.rewritten.json")).toContain(
      "is not assignable to type 'string'",
    );
  });
});

type DeclWithValue = AppSpec.Decl & { declValue: { [k: string]: unknown } };

function groupByType(
  decls: AppSpec.Decl[],
): Record<string, DeclWithValue[] | undefined> {
  const out: Record<string, DeclWithValue[]> = {};
  for (const decl of decls) {
    const list = out[decl.declType] ?? (out[decl.declType] = []);
    list.push(decl as DeclWithValue);
  }
  return out;
}

function writePackageJson(tempDir: string): void {
  writeFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({ type: "module" }),
  );
}

async function runSpecThroughRunTs(
  tempDir: string,
  sourceFileName: string,
  sourceText: string,
  outputFileName: string,
): Promise<AppSpec.Decl[]> {
  const sourcePath = join(tempDir, sourceFileName);
  const rewrittenFileName = sourceFileName.replace(/\.ts$/, ".rewritten.ts");
  const rewrittenPath = join(tempDir, rewrittenFileName);

  writeFileSync(sourcePath, sourceText, "utf8");
  await runWaspConfig(["node", "run.js", "rewrite", sourcePath, rewrittenPath]);

  const rewrittenSource = readFileSync(rewrittenPath, "utf8");
  expect(rewrittenSource).not.toMatch(/from\s+"\.\/src\//);

  const outDir = sourceFileName.replace(/\.ts$/, "-dist");
  const tsconfigFileName = sourceFileName.replace(/\.ts$/, ".tsconfig.json");
  writeTsConfig(tempDir, tsconfigFileName, rewrittenFileName, {
    noEmit: false,
    outDir,
  });
  runTsc(tempDir, tsconfigFileName);

  const compiledPath = join(
    tempDir,
    outDir,
    rewrittenFileName.replace(/\.ts$/, ".js"),
  );
  const outputPath = join(tempDir, outputFileName);
  await runWaspConfig(["node", "run.js", compiledPath, outputPath, "[]"]);

  return JSON.parse(readFileSync(outputPath, "utf8")) as AppSpec.Decl[];
}

function writeTsConfig(
  tempDir: string,
  filename: string,
  include: string,
  options: { noEmit?: boolean; outDir?: string } = {},
): void {
  writeFileSync(
    join(tempDir, filename),
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "bundler",
        strict: true,
        noEmit: options.noEmit ?? true,
        ...(options.outDir ? { outDir: options.outDir } : {}),
      },
      include: [include],
    }),
  );
}

function runTsc(cwd: string, project: string): void {
  execFileSync(process.execPath, [tscPath, "-p", project], {
    cwd,
    encoding: "utf8",
    stdio: "pipe",
  });
}

function runTscExpectingError(cwd: string, project: string): string {
  try {
    runTsc(cwd, project);
  } catch (error) {
    const processError = error as { stdout?: string; stderr?: string };
    return `${processError.stdout ?? ""}${processError.stderr ?? ""}`;
  }

  throw new Error("Expected TypeScript to reject the rewritten spec.");
}
