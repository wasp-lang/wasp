import { describe, expect, test } from "vitest";
import { ensureSourceAwareRefImport } from "../../../src/spec-pipeline/transform/ensureSourceAwareRefImport.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

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

  test("throws when there is no spec package import", () => {
    const localSpecApiImport =
      "file:///repo/waspc/data/packages/spec/src/spec/publicApi/index.ts";
    const sourceText = [
      `import { refImport, page } from ${JSON.stringify(localSpecApiImport)};`,
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

  test("uses a type-only spec package import as the helper anchor", () => {
    const sourceText = `import type { RefImport } from "@wasp.sh/spec";\n`;

    expect(transform(sourceText)).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import type { RefImport } from "@wasp.sh/spec";`,
        `import { makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        ``,
      ].join("\n"),
    });
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

  test("injects refImport for spec package value imports", () => {
    expect(transform(`import { app } from "@wasp.sh/spec";\n`)).toEqual({
      refImportName: "refImport",
      sourceText: [
        `import { app, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        ``,
      ].join("\n"),
    });
  });

  function transform(sourceText: string) {
    return ensureSourceAwareRefImport({ sourceText, sourcePath });
  }
});
