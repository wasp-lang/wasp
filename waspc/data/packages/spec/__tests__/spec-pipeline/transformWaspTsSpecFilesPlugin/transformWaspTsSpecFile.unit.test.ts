import { describe, expect, test } from "vitest";
import { transformWaspTsSpecFile_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/transformWaspTsSpecFile.js";
import * as Fixtures from "../../spec/testFixtures.js";

describe("transformWaspTsSpecFile_mutate", () => {
  test("adds a source-aware ref helper without a public spec package import", () => {
    const output = transform(`const title = "Demo";\n`);

    expect(output).toContain(
      `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
    );
    expect(output).toContain(
      `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
    );
    expect(output).toContain(`const title = "Demo";`);
  });

  test("rewrites public ref imports while preserving remaining spec imports", () => {
    const output = transform(
      [
        `import { type RefObject, ref as appRef, page } from "@wasp.sh/spec";`,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );

    expectInOrder(output, [
      `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
      `const appRef = _waspMakeRef("file:///project/main.wasp.ts");`,
      `import { type RefObject, page } from "@wasp.sh/spec";`,
      `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
    ]);
    expect(output).not.toContain(`ref as appRef`);
  });

  test("lowers ref imports through the source-aware helper", () => {
    const output = transform(
      [
        `import MainPage from "../MainPage" with { type: "ref" };`,
        `import { getTasks, archive as archiveTask } from "./operations" with { type: "ref" };`,
        `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
        ``,
      ].join("\n"),
      "/project/src/features/home.wasp.ts",
    );

    expectInOrder(output, [
      `const ref = _waspMakeRef("file:///project/src/features/home.wasp.ts");`,
      `const MainPage = ref({ importDefault: "MainPage", from: "../MainPage" });`,
      `const getTasks = ref({ import: "getTasks", from: "./operations" });`,
      `const archiveTask = ref({ import: "archive", from: "./operations", alias: "archiveTask" });`,
      `const adminOperations = new Proxy({}, { get: (_t, k) => ref({ import: String(k), from: "../adminOperations", alias: "adminOperations_" + String(k) }) }) as Record<string, ReturnType<typeof ref>>;`,
    ]);
    expect(output).not.toContain(`with { type: "ref" }`);
    expect(output).not.toContain(`from: "@src`);
  });

  test("generates helper names that do not collide with top-level bindings", () => {
    const output = transform(
      [
        `const ref = "taken";`,
        `const ref1 = "taken";`,
        `const _waspMakeRef = "taken";`,
        `import MainPage from "./src/MainPage" with { type: "ref" };`,
        ``,
      ].join("\n"),
    );

    expectInOrder(output, [
      `import { _waspMakeRef as _waspMakeRef1 } from "@wasp.sh/spec/internal";`,
      `const ref2 = _waspMakeRef1("file:///project/main.wasp.ts");`,
      `const MainPage = ref2({ importDefault: "MainPage", from: "./src/MainPage" });`,
    ]);
  });
});

function transform(
  sourceText: string,
  sourcePath = Fixtures.MOCK_MAIN_WASP_TS_PATH,
): string {
  const source = createMutableSource(sourceText);

  transformWaspTsSpecFile_mutate(source, { sourceText, sourcePath });

  return source.toString();
}

function createMutableSource(sourceText: string): {
  prepend: (source: string) => void;
  remove: (start: number, end: number) => void;
  toString: () => string;
} {
  let prependedSource = "";
  const removals: { start: number; end: number }[] = [];

  return {
    prepend: (source) => {
      prependedSource = source + prependedSource;
    },
    remove: (start, end) => {
      removals.push({ start, end });
    },
    toString: () => {
      const remainingSource = removals
        .toSorted((a, b) => b.start - a.start)
        .reduce(
          (source, removal) =>
            source.slice(0, removal.start) + source.slice(removal.end),
          sourceText,
        );

      return prependedSource + remainingSource;
    },
  };
}

function expectInOrder(source: string, parts: string[]): void {
  let cursor = 0;

  for (const part of parts) {
    const index = source.indexOf(part, cursor);
    expect(index).toBeGreaterThanOrEqual(0);
    cursor = index + part.length;
  }
}
