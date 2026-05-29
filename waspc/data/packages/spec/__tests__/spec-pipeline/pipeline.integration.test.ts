import { mkdirSync, mkdtempSync, realpathSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { dirname, join } from "path";
import { pathToFileURL } from "url";
import { describe, expect, test } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";

// We use the absolute file:// URL of the local @wasp.sh/spec source so the
// compiled spec does not rely on node_modules resolution from a temp dir.
const waspSpecEntryUrl = pathToFileURL(
  join(__dirname, "..", "..", "src", "spec", "publicApi", "index.ts"),
).href;

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
        `import { archive as archiveTask } from "../adminOperations" with { type: "ref" };`,
        ``,
        `export const splitTitle = "Split Demo";`,
        `export const archiveAction = action(archiveTask, { entities: [] });`,
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
      expect.objectContaining({ declType: "Action", declName: "archiveTask" }),
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
