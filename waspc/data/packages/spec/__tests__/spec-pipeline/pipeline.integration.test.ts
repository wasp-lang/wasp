import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as url from "node:url";
import { afterAll, beforeAll, describe, expect, test } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

// We use the absolute file:// URL of the local @wasp.sh/spec source so the
// compiled spec does not rely on node_modules resolution from a temp dir for
// public APIs.
const waspSpecEntryUrl = url.pathToFileURL(
  path.join(__dirname, "..", "..", "src", "spec", "publicApi", "index.ts"),
).href;

let disposeRuntimeSpecInternalPackageShim = (): void => {};

beforeAll(() => {
  disposeRuntimeSpecInternalPackageShim = writeSpecInternalPackageShim(
    path.join(__dirname, "..", ".."),
  );
});

afterAll(() => {
  disposeRuntimeSpecInternalPackageShim();
});

describe("Wasp TS spec pipeline", () => {
  test("analyzes split specs with lowered ref imports", async () => {
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
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { page } from ${JSON.stringify(waspSpecEntryUrl)};`,
        `import MainPage from "../MainPage" with { type: "ref" };`,
        ``,
        `export const homePage = page(MainPage);`,
      ].join("\n"),
    );
    project.writeProjectFile(
      "src/features/tasks.wasp.ts",
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { action } from ${JSON.stringify(waspSpecEntryUrl)};`,
        `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
        ``,
        `export const splitTitle = "Split Demo";`,
        `export const archiveAction = action(adminOperations.archive, { entities: [] });`,
      ].join("\n"),
    );

    const decls = await project.analyzeSpec(
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
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

  test("resolves ref imports when the project root is reached through a symlink", async () => {
    using project = makeSymlinkTempProject("wasp-spec-pipeline-symlink-");

    project.writeProjectFile(
      "src/MainPage.ts",
      `export default function MainPage() { return null; }\n`,
    );

    // The bundler reports canonical (symlink-resolved) module ids, while the
    // project root is given as a symlinked path. Mapping the source-aware ref
    // object must still place it inside src/ instead of rejecting it as escaping
    // src/.
    const decls = await project.analyzeSpec(
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app, page } from ${JSON.stringify(waspSpecEntryUrl)};`,
        `import MainPage from "./src/MainPage" with { type: "ref" };`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  decls: [page(MainPage)],`,
        `});`,
      ].join("\n"),
    );

    expect(decls).toContainEqual(
      expect.objectContaining({ declType: "Page", declName: "MainPage" }),
    );
  });

  test("surfaces type errors in the spec as a SpecUserError with formatted diagnostics", async () => {
    using project = makeTempProject("wasp-spec-pipeline-type-error-");

    const result = project.analyzeSpec(
      [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
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

    await expect(result).rejects.toThrow(SpecUserError);
    await expect(result).rejects.toThrow(
      "Type 'number' is not assignable to type 'string'",
    );
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

function makeSymlinkTempProject(prefix: string): TempProject {
  const realRootDir = fs.mkdtempSync(path.join(os.tmpdir(), prefix));
  const symlinkRootDir = `${realRootDir}-link`;
  fs.symlinkSync(realRootDir, symlinkRootDir, "dir");

  return scaffoldProject({
    projectRootDir: symlinkRootDir,
    dispose: () => {
      fs.rmSync(symlinkRootDir, { force: true });
      fs.rmSync(realRootDir, { recursive: true, force: true });
    },
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
    JSON.stringify({ type: "module" }),
  );
  writeSpecInternalPackageShim(projectRootDir);
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

    analyzeSpec: async (sourceText: string) => {
      const sourcePath = path.join(projectRootDir, "main.wasp.ts");

      fs.writeFileSync(sourcePath, sourceText, "utf8");

      return analyzeApp({
        waspTsSpecPath: sourcePath,
        tsconfigPath,
        projectRootDir,
        entityNames: [],
      });
    },
  };
}

function writeSpecInternalPackageShim(nodeModulesOwnerDir: string): () => void {
  const packageRootDir = path.join(
    nodeModulesOwnerDir,
    "node_modules",
    "@wasp.sh",
    "spec",
  );

  if (fs.existsSync(packageRootDir)) {
    return () => {};
  }

  writeFile(
    path.join(packageRootDir, "package.json"),
    JSON.stringify({
      type: "module",
      exports: {
        "./internal": {
          types: "./internal.d.ts",
          default: "./internal.js",
        },
      },
    }),
  );
  writeFile(
    path.join(packageRootDir, "internal.js"),
    [
      `import { fileURLToPath } from "node:url";`,
      ``,
      `export function _waspMakeRef(importingFileUrl) {`,
      `  const sourceFilePath = fileURLToPath(importingFileUrl);`,
      ``,
      `  return (descriptor) => ({ ...descriptor, kind: "refObject", sourceFilePath });`,
      `}`,
      ``,
    ].join("\n"),
  );
  writeFile(
    path.join(packageRootDir, "internal.d.ts"),
    `export declare function _waspMakeRef(importingFileUrl: string): (descriptor: any) => any;\n`,
  );

  return () => {
    fs.rmSync(packageRootDir, { recursive: true, force: true });

    try {
      fs.rmdirSync(path.dirname(packageRootDir));
    } catch {
      // The scope directory can contain unrelated packages.
    }
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
