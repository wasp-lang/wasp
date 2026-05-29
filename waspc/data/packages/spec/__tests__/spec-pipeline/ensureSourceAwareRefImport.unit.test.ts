import { describe, expect, test } from "vitest";
import { ensureSourceAwareRefImport } from "../../src/spec-pipeline/ensureSourceAwareRefImport.js";

describe("ensureSourceAwareRefImport", () => {
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
        `import { page, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
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
        `import { page, makeRefImport } from "@wasp.sh/spec";`,
        `const ref = makeRefImport(import.meta.url);`,
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
        `import { type RefImport, page, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test("leaves type-only imports unchanged", () => {
    const sourceText = `import type { RefImport } from "@wasp.sh/spec";\n`;

    expect(transform(sourceText)).toEqual({ sourceText });
  });

  test("reuses an existing makeRefImport import", () => {
    expect(
      transform(
        [
          `import { makeRefImport, refImport, page } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { makeRefImport, page } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test("reuses an aliased makeRefImport import", () => {
    expect(
      transform(
        [
          `import { makeRefImport as makeRef, refImport as ref, page } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refImportName: "ref",
      sourceText: [
        `import { makeRefImport as makeRef, page } from "@wasp.sh/spec";`,
        `const ref = makeRef(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  test("injects refImport when required", () => {
    expect(
      transform(`import { app } from "@wasp.sh/spec";\n`, { required: true }),
    ).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { app, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  function transform(
    sourceText: string,
    { required }: { required: boolean } = { required: false },
  ) {
    return ensureSourceAwareRefImport({ sourceText, sourcePath, required });
  }
});
