import { describe, expect, test } from "vitest";
import { addSourceAwareRefImport } from "../../../src/spec-pipeline/transform/addSourceAwareRefImport.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("addSourceAwareRefImport", () => {
  const sourcePath = "/project/main.wasp.ts";

  test("rewrites explicit refImport imports", () => {
    expect(
      transform(
        [
          `import { refImport, page } from "@wasp.sh/spec";`,
          `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { page } from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const refImport = _waspMakeRef(import.meta.url);`,
        `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
  });

  test("throws when there is no spec package import", () => {
    const localSpecPackageImport =
      "file:///repo/waspc/data/packages/spec/src/spec/publicApi/index.ts";
    const sourceText = [
      `import { refImport, page } from ${JSON.stringify(localSpecPackageImport)};`,
      `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
      ``,
    ].join("\n");

    expect(() => transform(sourceText)).toThrowError(SpecUserError);
  });

  test("preserves a refImport alias", () => {
    expect(
      transform(
        [
          `import { refImport as ref, page } from "@wasp.sh/spec";`,
          `const MainPage = ref({ importDefault: "MainPage", from: "./MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refImportName: "ref",
      sourceText: [
        `import { page } from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `const MainPage = ref({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
  });

  test("preserves type specifiers in mixed imports", () => {
    expect(
      transform(
        [
          `import { type RefImport, refImport, page } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { type RefImport, page } from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const refImport = _waspMakeRef(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test("uses a type-only spec package import as the helper anchor", () => {
    const sourceText = `import type { RefImport } from "@wasp.sh/spec";\n`;

    expect(transform(sourceText)).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import type { RefImport } from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const refImport = _waspMakeRef(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test.each([
    [`import { _waspMakeRef, refImport, page } from "@wasp.sh/spec";\n`],
    [
      `import { _waspMakeRef as makeRef, refImport, page } from "@wasp.sh/spec";\n`,
    ],
    [`import { type _waspMakeRef, refImport, page } from "@wasp.sh/spec";\n`],
    [`import type { _waspMakeRef } from "@wasp.sh/spec";\n`],
  ])("rejects internal helper import: %s", (sourceText) => {
    expect(() => transform(sourceText)).toThrowError(SpecUserError);
  });

  test("adds refImport helper for spec package value imports", () => {
    expect(transform(`import { app } from "@wasp.sh/spec";\n`)).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { app } from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const refImport = _waspMakeRef(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test("allows namespace spec package imports", () => {
    expect(transform(`import * as wasp from "@wasp.sh/spec";\n`)).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import * as wasp from "@wasp.sh/spec";`,
        `import { _waspMakeRef } from "@wasp.sh/spec";`,
        `const refImport = _waspMakeRef(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  function transform(sourceText: string) {
    return addSourceAwareRefImport({ sourceText, sourcePath });
  }
});
