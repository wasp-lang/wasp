import { describe, expect, test } from "vitest";
import { planLowerImports } from "../../../src/spec-pipeline/lowerImportsPlugin/plan.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("planLowerImports", () => {
  const projectRootDir = "/project";
  const importingFilePath = `${projectRootDir}/main.wasp.ts`;

  test('plans a default ref import as an `import: "default"` binding', () => {
    const plan = planLowerImports({
      sourceText: `import MainPage from "./src/MainPage" with { type: "ref" };\n`,
      importingFilePath,
      projectRootDir,
    });

    expect(plan).toHaveLength(1);
    expect(plan[0]?.from).toBe("@src/MainPage");
    expect(plan[0]?.bindings).toEqual([
      { import: "default", alias: "MainPage" },
    ]);
  });

  test("plans named ref imports preserving the imported name as the alias", () => {
    const plan = planLowerImports({
      sourceText: `import { getTasks, createTask } from "./src/operations" with { type: "ref" };\n`,
      importingFilePath,
      projectRootDir,
    });

    expect(plan[0]?.bindings).toEqual([
      { import: "getTasks", alias: "getTasks" },
      { import: "createTask", alias: "createTask" },
    ]);
  });

  test("plans aliased named ref imports with the local name as the alias", () => {
    const plan = planLowerImports({
      sourceText: `import { archive as archiveTask } from "./src/operations" with { type: "ref" };\n`,
      importingFilePath,
      projectRootDir,
    });

    expect(plan[0]?.bindings).toEqual([
      { import: "archive", alias: "archiveTask" },
    ]);
  });

  test("plans a combined default + named ref import in a single statement", () => {
    const plan = planLowerImports({
      sourceText: `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };\n`,
      importingFilePath,
      projectRootDir,
    });

    expect(plan[0]?.bindings).toEqual([
      { import: "default", alias: "MainPage" },
      { import: "Helper", alias: "Helper" },
    ]);
  });

  test("resolves ref imports relative to the importing file's directory", () => {
    const plan = planLowerImports({
      sourceText: `import MainPage from "./MainPage" with { type: "ref" };\n`,
      importingFilePath: `${projectRootDir}/src/features/home.wasp.ts`,
      projectRootDir,
    });

    expect(plan[0]?.from).toBe("@src/features/MainPage");
  });

  test("plans nothing for non-ref imports and re-exports", () => {
    expect(
      planLowerImports({
        sourceText: [
          `import { app } from "@wasp.sh/spec";`,
          `import z from "zod";`,
          `import helper from "./helpers";`,
          `import MainPage from "@src/MainPage";`,
          `export { helper } from "./helpers";`,
          ``,
        ].join("\n"),
        importingFilePath,
        projectRootDir,
      }),
    ).toEqual([]);
  });

  test("plans only the ref imports in a mixed file", () => {
    const plan = planLowerImports({
      sourceText: [
        `import { app } from "@wasp.sh/spec";`,
        `import MainPage from "./src/MainPage" with { type: "ref" };`,
        `import { getTasks } from "./src/operations" with { type: "ref" };`,
        `import helper from "./helpers";`,
        ``,
      ].join("\n"),
      importingFilePath,
      projectRootDir,
    });

    expect(plan).toHaveLength(2);
    expect(plan[0]?.from).toBe("@src/MainPage");
    expect(plan[1]?.from).toBe("@src/operations");
  });

  test("records the source span of each ref import for replacement", () => {
    const sourceText = [
      `import { app } from "@wasp.sh/spec";`,
      `import MainPage from "./src/MainPage" with { type: "ref" };`,
      ``,
    ].join("\n");

    const plan = planLowerImports({
      sourceText,
      importingFilePath,
      projectRootDir,
    });

    const refImportStatement = `import MainPage from "./src/MainPage" with { type: "ref" };`;
    const expectedStart = sourceText.indexOf(refImportStatement);

    expect(plan[0]?.start).toBe(expectedStart);
    expect(plan[0]?.end).toBe(expectedStart + refImportStatement.length);
  });

  test("rejects namespace ref imports with a SpecUserError", () => {
    expect(() =>
      planLowerImports({
        sourceText: `import * as ops from "./src/operations" with { type: "ref" };\n`,
        importingFilePath,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
  });
});
