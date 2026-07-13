import { mkdirSync, mkdtempSync, realpathSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { describe, expect, test } from "vitest";
import {
  assertHasDefaultExport,
  getSourceEntries,
  parseArgs,
} from "../src/index.js";

describe("parseArgs", () => {
  test("parses a single build", () => {
    const moduleDir = makeModuleDir();

    expect(parseArgs(["--module-dir", moduleDir])).toEqual({
      moduleDir: realpathSync(moduleDir),
    });
  });

  test("rejects unknown arguments", () => {
    expect(() => parseArgs(["--module-dir", "/module", "--bad"])).toThrow(
      /Usage:/,
    );
  });
});

function makeModuleDir(): string {
  return mkdtempSync(path.join(tmpdir(), "wasp-module-builder-test-"));
}

describe("assertHasDefaultExport", () => {
  test("accepts a default export", () => {
    expect(() =>
      assertHasDefaultExport(
        "/module/module.wasp.ts",
        `export default function getModuleSpec(options) { return []; }`,
      ),
    ).not.toThrow();
  });

  test("rejects missing default export", () => {
    expect(() =>
      assertHasDefaultExport(
        "/module/module.wasp.ts",
        `export const moduleSpec = [];`,
      ),
    ).toThrow(/must default export/);
  });
});

describe("getSourceEntries", () => {
  test("maps src files to package subpath entries", () => {
    const moduleDir = makeModuleDir();
    mkdirSync(path.join(moduleDir, "src", "pages"), { recursive: true });
    writeFileSync(path.join(moduleDir, "src", "MainPage.tsx"), "");
    writeFileSync(path.join(moduleDir, "src", "queries.ts"), "");
    writeFileSync(path.join(moduleDir, "src", "ignored.d.ts"), "");
    writeFileSync(path.join(moduleDir, "src", "ignored.wasp.ts"), "");
    writeFileSync(path.join(moduleDir, "src", "pages", "Details.tsx"), "");

    expect(getSourceEntries(moduleDir)).toEqual({
      MainPage: "./src/MainPage.tsx",
      "pages/Details": "./src/pages/Details.tsx",
      queries: "./src/queries.ts",
    });
  });

  test("rejects entries that collide with the compiled spec", () => {
    const moduleDir = makeModuleDir();
    mkdirSync(path.join(moduleDir, "src"));
    writeFileSync(path.join(moduleDir, "src", "Spec.ts"), "");

    expect(() => getSourceEntries(moduleDir)).toThrow(
      /conflicts with the compiled module spec/,
    );
  });
});
