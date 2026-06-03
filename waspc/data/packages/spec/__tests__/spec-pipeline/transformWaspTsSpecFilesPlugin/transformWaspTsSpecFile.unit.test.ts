import { describe, expect, test } from "vitest";
import { transformWaspTsSpecFile_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/transformWaspTsSpecFile.js";
import * as Fixtures from "../../spec/testFixtures.js";

describe("transformWaspTsSpecFile_mutate", () => {
  test("adds a source-aware ref helper without a public spec package import", () => {
    expect(transform([`const title = "Demo";`, ``].join("\n"))).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const title = "Demo";`,
        ``,
      ].join("\n"),
    );
  });

  test("rewrites public ref imports while preserving remaining spec imports", () => {
    expect(
      transform(
        [
          `import { type RefObject, ref as appRef, page } from "@wasp.sh/spec";`,
          `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const appRef = _waspMakeRef("file:///project/main.wasp.ts");`,
        `import { type RefObject, page } from "@wasp.sh/spec";`,
        ``,
        ``,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test.for([
    {
      caseName: "a default ref import",
      sourcePath: "/project/src/features/home.wasp.ts",
      sourceLines: [
        `import MainPage from "../MainPage" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/src/features/home.wasp.ts");`,
        `const MainPage = ref({ importDefault: "MainPage", from: "../MainPage" });`,
        ``,
        ``,
      ],
    },
    {
      caseName: "named ref imports",
      sourceLines: [
        `import { getTasks, createTask } from "./operations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const getTasks = ref({ import: "getTasks", from: "./operations" });`,
        `const createTask = ref({ import: "createTask", from: "./operations" });`,
        ``,
        ``,
      ],
    },
    {
      caseName: "aliased named ref imports",
      sourceLines: [
        `import { archive as archiveTask } from "./operations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const archiveTask = ref({ import: "archive", from: "./operations", alias: "archiveTask" });`,
        ``,
        ``,
      ],
    },
    {
      caseName: "namespace ref imports",
      sourceLines: [
        `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const adminOperations = new Proxy({}, { get: (_t, k) => ref({ import: String(k), from: "../adminOperations", alias: "adminOperations_" + String(k) }) }) as Record<string, ReturnType<typeof ref>>;`,
        ``,
        ``,
      ],
    },
    {
      caseName: "a combined default and named ref import",
      sourceLines: [
        `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const MainPage = ref({ importDefault: "MainPage", from: "./src/MainPage" });`,
        `const Helper = ref({ import: "Helper", from: "./src/MainPage" });`,
        ``,
        ``,
      ],
    },
  ])("lowers $caseName", ({ sourceLines, expectedLines, sourcePath }) => {
    expect(transform(sourceLines.join("\n"), sourcePath)).toBe(
      expectedLines.join("\n"),
    );
  });

  test("lowers only ref imports in a mixed file", () => {
    expect(
      transform(
        [
          `import { app } from "@wasp.sh/spec";`,
          `import z from "zod";`,
          `import helper from "./helpers";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          `export { helper } from "./helpers";`,
          `const title = "Demo";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `const MainPage = ref({ importDefault: "MainPage", from: "./src/MainPage" });`,
        `import { app } from "@wasp.sh/spec";`,
        `import z from "zod";`,
        `import helper from "./helpers";`,
        ``,
        `export { helper } from "./helpers";`,
        `const title = "Demo";`,
        ``,
      ].join("\n"),
    );
  });

  test("generates helper names that do not collide with direct top-level bindings", () => {
    expect(
      transform(
        [
          `const ref = "taken";`,
          `const ref1 = "taken";`,
          `const _waspMakeRef = "taken";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef as _waspMakeRef1 } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref2 = _waspMakeRef1("file:///project/main.wasp.ts");`,
        `const MainPage = ref2({ importDefault: "MainPage", from: "./src/MainPage" });`,
        `const ref = "taken";`,
        `const ref1 = "taken";`,
        `const _waspMakeRef = "taken";`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  test("does not treat type-only imports as helper name collisions", () => {
    expect(
      transform(
        [
          `import type { ref, _waspMakeRef } from "./types";`,
          `const title = "Demo";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef("file:///project/main.wasp.ts");`,
        `import type { ref, _waspMakeRef } from "./types";`,
        `const title = "Demo";`,
        ``,
      ].join("\n"),
    );
  });

  test("generates helper names around destructured and declared top-level value collisions", () => {
    expect(
      transform(
        [
          `import { ref } from "./helpers";`,
          `const { ref1, nested: { ref2 }, ...ref3 } = helpers;`,
          `function ref4() {}`,
          `class _waspMakeRef {}`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef as _waspMakeRef1 } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref5 = _waspMakeRef1("file:///project/main.wasp.ts");`,
        `const MainPage = ref5({ importDefault: "MainPage", from: "./src/MainPage" });`,
        `import { ref } from "./helpers";`,
        `const { ref1, nested: { ref2 }, ...ref3 } = helpers;`,
        `function ref4() {}`,
        `class _waspMakeRef {}`,
        ``,
        ``,
      ].join("\n"),
    );
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
