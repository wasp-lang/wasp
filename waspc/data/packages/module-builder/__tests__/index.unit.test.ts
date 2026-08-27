import { mkdtempSync, realpathSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { describe, expect, test } from "vitest";
import { assertHasDefaultExport, parseArgs } from "../src/index.js";

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
