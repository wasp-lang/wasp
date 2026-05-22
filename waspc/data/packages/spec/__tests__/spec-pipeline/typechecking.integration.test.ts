import { writeFileSync } from "fs";
import { join } from "path";
import { pathToFileURL } from "url";
import { afterEach, describe, expect, test, vi } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import {
  makeTempProject,
  writeProjectFile,
  writeTsConfig,
  writeUserSourceFiles,
} from "./testHelpers.js";

const waspSpecEntryUrl = pathToFileURL(
  join(__dirname, "..", "..", "src", "spec", "publicApi", "index.ts"),
).href;

describe("typechecking via the full spec pipeline", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  test("rejects a .wasp.ts spec with a type error in the spec body", async () => {
    const tempDir = makeTempProject("typecheck-pipeline-body-error-");
    silenceConsoleError();

    await expect(
      analyzeSpec({
        tempDir,
        specFileName: "main.wasp.ts",
        sourceText: [
          `// @ts-ignore: This test imports the local TS source through Vitest.`,
          `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
          ``,
          `const _bad: string = 123;`,
          ``,
          `export default app({`,
          `  name: "demo",`,
          `  title: "Demo",`,
          `  wasp: { version: "^0.16.0" },`,
          `  parts: [],`,
          `});`,
        ].join("\n"),
      }),
    ).rejects.toThrowError(SpecUserError);
  });

  test("typechecks the lowered ref-import form, not the original import", async () => {
    const tempDir = makeTempProject("typecheck-pipeline-lowered-form-");
    writeUserSourceFiles(tempDir);
    silenceConsoleError();

    const decls = await analyzeSpec({
      tempDir,
      specFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app, page } from ${JSON.stringify(waspSpecEntryUrl)};`,
        `import MainPage from "./src/MainPage" with { type: "ref" };`,
        ``,
        // Accessing `.from` only typechecks on the lowered ExtImport shape;
        // the original default export is a React component (`() => null`).
        `const _loweredProof: "@src/MainPage" = MainPage.from;`,
        ``,
        `export default app({`,
        `  name: "demo",`,
        `  title: "Demo",`,
        `  wasp: { version: "^0.16.0" },`,
        `  parts: [page(MainPage)],`,
        `});`,
      ].join("\n"),
    });

    expect(decls).toContainEqual(expect.objectContaining({ declType: "App" }));
  });

  test("rejects a spec that uses a ref-imported value as a callable", async () => {
    const tempDir = makeTempProject("typecheck-pipeline-callable-ref-");
    writeUserSourceFiles(tempDir);
    silenceConsoleError();

    await expect(
      analyzeSpec({
        tempDir,
        specFileName: "main.wasp.ts",
        sourceText: [
          `// @ts-ignore: This test imports the local TS source through Vitest.`,
          `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          ``,
          // After lowering, MainPage is an object const — not callable.
          // This proves the typechecker sees the lowered form, not the original.
          `MainPage();`,
          ``,
          `export default app({`,
          `  name: "demo",`,
          `  title: "Demo",`,
          `  wasp: { version: "^0.16.0" },`,
          `  parts: [],`,
          `});`,
        ].join("\n"),
      }),
    ).rejects.toThrowError(SpecUserError);
  });

  test("rejects a type error in a helper file imported from the spec", async () => {
    const tempDir = makeTempProject("typecheck-pipeline-helper-error-");
    silenceConsoleError();

    writeProjectFile(
      tempDir,
      "helpers/title.ts",
      `export const title: string = 123 as unknown as string;\nexport const broken: string = 123;\n`,
    );

    await expect(
      analyzeSpec({
        tempDir,
        specFileName: "main.wasp.ts",
        sourceText: [
          `// @ts-ignore: This test imports the local TS source through Vitest.`,
          `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
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
    ).rejects.toThrowError(SpecUserError);
  });

  test("passes a well-typed minimal spec end-to-end", async () => {
    const tempDir = makeTempProject("typecheck-pipeline-clean-");

    const decls = await analyzeSpec({
      tempDir,
      specFileName: "main.wasp.ts",
      sourceText: [
        `// @ts-ignore: This test imports the local TS source through Vitest.`,
        `import { app } from ${JSON.stringify(waspSpecEntryUrl)};`,
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
});

async function analyzeSpec({
  tempDir,
  specFileName,
  sourceText,
}: {
  tempDir: string;
  specFileName: string;
  sourceText: string;
}): Promise<AppSpec.Decl[]> {
  const sourcePath = join(tempDir, specFileName);
  const tsconfigPath = join(
    tempDir,
    specFileName.replace(/\.ts$/, ".tsconfig.json"),
  );

  writeFileSync(sourcePath, sourceText, "utf8");
  writeTsConfig(tsconfigPath, specFileName);

  return analyzeApp({
    waspTsSpecPath: sourcePath,
    tsconfigPath,
    projectRootDir: tempDir,
    entityNames: [],
  });
}

function silenceConsoleError(): ReturnType<typeof vi.spyOn> {
  return vi.spyOn(console, "error").mockImplementation(() => {});
}
