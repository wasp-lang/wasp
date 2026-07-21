import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { afterAll, afterEach, beforeAll, describe, expect, test } from "vitest";

type RollupPackages = {
  discoverWaspModulePackages(appRootDir: string): string[];
  shouldExternalize(
    importPath: string,
    packagesToBundle: ReadonlySet<string>,
  ): boolean;
};

const appRootDirs: string[] = [];
const helperDir = mkdtempSync(path.join(tmpdir(), "wasp-rollup-helper-"));
let rollupPackages: RollupPackages;

beforeAll(async () => {
  const sourcePath = fileURLToPath(
    new URL(
      "../../../Generator/templates/server/rollupPackages.js",
      import.meta.url,
    ),
  );
  const helperPath = path.join(helperDir, "rollupPackages.mjs");
  writeFileSync(helperPath, readFileSync(sourcePath));
  rollupPackages = (await import(
    /* @vite-ignore */ pathToFileURL(helperPath).href
  )) as RollupPackages;
});

afterAll(() => {
  rmSync(helperDir, { recursive: true, force: true });
});

afterEach(() => {
  for (const appRootDir of appRootDirs.splice(0)) {
    rmSync(appRootDir, { recursive: true, force: true });
  }
});

describe("discoverWaspModulePackages", () => {
  test("finds only direct dependencies with a wasp.module object", () => {
    const appRootDir = makeAppRoot({
      dependencies: {
        "@acme/module": "1.0.0",
        regular: "1.0.0",
        "invalid-module": "1.0.0",
      },
      devDependencies: {
        "dev-module": "1.0.0",
      },
    });
    writePackageJson(appRootDir, "@acme/module", {
      wasp: { module: {} },
    });
    writePackageJson(appRootDir, "regular", {
      peerDependencies: { wasp: "*" },
    });
    writePackageJson(appRootDir, "invalid-module", {
      wasp: { module: true },
    });
    writePackageJson(appRootDir, "dev-module", {
      wasp: { module: {} },
    });
    writePackageJson(
      path.join(appRootDir, "node_modules", "@acme", "module"),
      "nested-module",
      { wasp: { module: {} } },
    );

    expect(rollupPackages.discoverWaspModulePackages(appRootDir)).toEqual([
      "@acme/module",
    ]);
  });
});

describe("shouldExternalize", () => {
  const packagesToBundle = new Set(["wasp", "@acme/module"]);

  test.each([
    ["wasp/server/jobs", false],
    ["@acme/module/server", false],
    ["quote-lib", true],
    ["node:fs", true],
    ["./local.js", false],
    ["#internal", false],
    [path.resolve("absolute.js"), false],
  ])("classifies %s", (importPath, expected) => {
    expect(rollupPackages.shouldExternalize(importPath, packagesToBundle)).toBe(
      expected,
    );
  });
});

function makeAppRoot(packageJson: object): string {
  const appRootDir = mkdtempSync(path.join(tmpdir(), "wasp-rollup-packages-"));
  appRootDirs.push(appRootDir);
  writeFileSync(
    path.join(appRootDir, "package.json"),
    JSON.stringify(packageJson),
  );
  return appRootDir;
}

function writePackageJson(
  appRootDir: string,
  packageName: string,
  packageJson: object,
): void {
  const packageDir = path.join(appRootDir, "node_modules", packageName);
  mkdirSync(packageDir, { recursive: true });
  writeFileSync(
    path.join(packageDir, "package.json"),
    JSON.stringify(packageJson),
  );
}
