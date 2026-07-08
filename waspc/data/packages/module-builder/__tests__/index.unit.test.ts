import { mkdirSync, mkdtempSync, realpathSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import { describe, expect, test } from "vitest";
import {
  assertHasDefaultExport,
  getLooseModuleSpecTypes,
  getModulePackageRefSource,
  getSourceEntries,
  parseArgs,
  transformModuleRefImports_mutate,
} from "../src/index.js";

describe("parseArgs", () => {
  test("parses a single build", () => {
    const moduleDir = makeModuleDir();

    expect(parseArgs(["--module-dir", moduleDir])).toEqual({
      moduleDir: realpathSync(moduleDir),
      watch: false,
    });
  });

  test("parses a watch build", () => {
    const moduleDir = makeModuleDir();

    expect(parseArgs(["--module-dir", moduleDir, "--watch"])).toEqual({
      moduleDir: realpathSync(moduleDir),
      watch: true,
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

describe("getModulePackageRefSource", () => {
  test("maps a relative src ref to a package subpath", () => {
    expect(
      getModulePackageRefSource({
        from: "./src/pages/MainPage",
        moduleDir: "/module",
        packageName: "@acme/fsm",
      }),
    ).toBe("@acme/fsm/pages/MainPage");
  });

  test("keeps existing package refs unchanged", () => {
    expect(
      getModulePackageRefSource({
        from: "@other/fsm/Widget",
        moduleDir: "/module",
        packageName: "@acme/fsm",
      }),
    ).toBe("@other/fsm/Widget");
  });

  test("rejects relative refs outside src", () => {
    expect(() =>
      getModulePackageRefSource({
        from: "./moduleHelper",
        moduleDir: "/module",
        packageName: "@acme/fsm",
      }),
    ).toThrow(/inside src/);
  });
});

describe("getLooseModuleSpecTypes", () => {
  test("loosens direct named exports", () => {
    expect(getLooseModuleSpecTypes(`export const moduleSpec = [];`)).toBe(
      [`declare const moduleSpec: any;`, `export { moduleSpec };`, ``].join(
        "\n",
      ),
    );
  });

  test("loosens named export lists", () => {
    expect(
      getLooseModuleSpecTypes(
        [
          `const moduleSpec = [];`,
          `const otherSpec = [];`,
          `export { moduleSpec, otherSpec as renamedSpec };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `declare const moduleSpec: any;`,
        `declare const renamedSpec: any;`,
        `export { moduleSpec, renamedSpec };`,
        ``,
      ].join("\n"),
    );
  });

  test("loosens default exports", () => {
    expect(getLooseModuleSpecTypes(`export default [];`)).toBe(
      [`declare const _default: any;`, `export default _default;`, ``].join(
        "\n",
      ),
    );
  });
});

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
    writeFileSync(path.join(moduleDir, "src", "pages", "Details.tsx"), "");

    expect(getSourceEntries(moduleDir)).toEqual({
      MainPage: "./src/MainPage.tsx",
      "pages/Details": "./src/pages/Details.tsx",
      queries: "./src/queries.ts",
    });
  });
});

describe("transformModuleRefImports_mutate", () => {
  test("reuses ref import lowering while mapping refs to package sources", () => {
    expect(
      transformModuleRefImports(
        [
          `import { page } from "@wasp.sh/spec";`,
          `import { MainPage } from "./src/pages/MainPage" with { type: "ref" };`,
          `export const spec = [page(MainPage)];`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { fileURLToPath } from "node:url";`,
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const ref = _waspMakeRef(fileURLToPath(import.meta.url));`,
        `const MainPage = ref({"import":"MainPage","alias":"MainPage","from":"@acme/fsm/pages/MainPage"});`,
        `import { page } from "@wasp.sh/spec";`,
        ``,
        `export const spec = [page(MainPage)];`,
        ``,
      ].join("\n"),
    );
  });

  test("uses collision-safe helper names from shared lowering", () => {
    expect(
      transformModuleRefImports(
        [
          `const ref = "local";`,
          `const fileURLToPath = "local";`,
          `const _waspMakeRef = "local";`,
          `import Page from "./src/Page" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toContain(
      `const ref_0 = _waspMakeRef_0(fileURLToPath_0(import.meta.url));`,
    );
  });
});

function transformModuleRefImports(source: string): string {
  const ast = parseAst(source, { lang: "ts" });
  const magicString = new RolldownMagicString(source);

  transformModuleRefImports_mutate(ast, magicString, {
    moduleDir: "/module",
    packageName: "@acme/fsm",
  });

  return magicString.toString();
}
