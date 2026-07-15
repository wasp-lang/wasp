import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import ts from "typescript";
import { expect, test, vi } from "vitest";
import { buildModule } from "../src/index.js";

// Each test scaffolds a module and runs real npm installs and tsdown builds,
// which exceed the default 5s timeout on slower CI runners.
vi.setConfig({ testTimeout: 60_000 });

test("a source-free build removes stale output and emits the compiled spec", async () => {
  const moduleDir = scaffoldModule();
  const staleOutputPath = path.join(moduleDir, "dist", "stale.js");
  mkdirSync(path.dirname(staleOutputPath), { recursive: true });
  writeFileSync(staleOutputPath, "stale");

  try {
    await buildModule(moduleDir);

    expect(existsSync(staleOutputPath)).toBe(false);
    expect(existsSync(specJavaScriptPath(moduleDir))).toBe(true);
    expect(readFileSync(specDeclarationPath(moduleDir), "utf8")).toContain(
      "prefix: string",
    );
    expect(existsSync(path.join(moduleDir, "dist", "__wasp"))).toBe(false);
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

test("compiled refs carry package-relative logical origins", async () => {
  const moduleDir = scaffoldModule();
  installSpecPackage(moduleDir);
  mkdirSync(path.join(moduleDir, "src"));
  mkdirSync(path.join(moduleDir, "features"));
  writeFileSync(
    path.join(moduleDir, "src", "queries.ts"),
    "export async function getTodoItems() { return document.title; }\n",
  );
  writeFileSync(
    path.join(moduleDir, "src", "options.tsx"),
    "export type ModuleOptions = { prefix: string };\n",
  );
  writeFileSync(
    path.join(moduleDir, "features", "declared-options.d.ts"),
    "export type DeclaredOptions = { enabled: boolean };\n",
  );
  writeFileSync(
    path.join(moduleDir, "features", "feature.wasp.ts"),
    [
      'import { query, type Spec } from "@wasp.sh/spec";',
      'import type { DeclaredOptions } from "./declared-options.js";',
      'import type { ModuleOptions } from "../src/options";',
      'import { getTodoItems } from "../src/queries" with { type: "ref" };',
      "export function getFeatureSpec(_options: ModuleOptions & DeclaredOptions): Spec {",
      "  return [query(getTodoItems)];",
      "}",
      "",
    ].join("\n"),
  );
  writeFileSync(
    path.join(moduleDir, "module.wasp.ts"),
    [
      'import type { Spec } from "@wasp.sh/spec";',
      'import { getFeatureSpec } from "./features/feature.wasp";',
      'import type { DeclaredOptions } from "./features/declared-options.js";',
      'import type { ModuleOptions } from "./src/options";',
      "export default function getModuleSpec(options: ModuleOptions & DeclaredOptions): Spec {",
      "  return getFeatureSpec(options);",
      "}",
      "",
    ].join("\n"),
  );

  try {
    await buildModule(moduleDir);

    const compiledSpec = readFileSync(specJavaScriptPath(moduleDir), "utf8");
    expect(compiledSpec).toContain('"packageName": "test-module"');
    expect(compiledSpec).toContain(
      '"specFilePath": "features/feature.wasp.ts"',
    );
    expect(compiledSpec).not.toContain(realpathSync(moduleDir));
    expect(compiledSpec).not.toContain('type:"ref"');
    expect(existsSync(path.join(moduleDir, "dist", "feature.wasp.js"))).toBe(
      false,
    );
    const specDeclaration = readFileSync(
      specDeclarationPath(moduleDir),
      "utf8",
    );
    expect(specDeclaration).toContain("prefix: string");
    expect(specDeclaration).toContain("enabled: boolean");
    expect(specDeclaration).not.toContain("./src/");
    expect(specDeclaration).not.toContain("./features/");
    expect(existsSync(path.join(moduleDir, "dist", "__wasp"))).toBe(false);
    expectDeclarationFilesResolve(moduleDir);

    const specModule: unknown = await import(
      pathToFileURL(specJavaScriptPath(moduleDir)).href
    );
    expect(specModule).toMatchObject({ default: expect.any(Function) });
    const getModuleSpec = (
      specModule as {
        default: (options: { prefix: string; enabled: boolean }) => unknown[];
      }
    ).default;
    expect(getModuleSpec({ prefix: "test", enabled: true })[0]).toMatchObject({
      kind: "query",
      fn: {
        kind: "refObject",
        import: "getTodoItems",
        from: "../src/queries",
        origin: {
          kind: "package",
          packageName: "test-module",
          specFilePath: "features/feature.wasp.ts",
        },
      },
    });
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

test("relative CSS imports stay verbatim and imported CSS files copy to dist", async () => {
  const moduleDir = scaffoldModule();
  mkdirSync(path.join(moduleDir, "src", "sub"), { recursive: true });
  writeFileSync(
    path.join(moduleDir, "src", "Widget.tsx"),
    ['import "./Widget.css";', 'export const widget = "widget";', ""].join(
      "\n",
    ),
  );
  writeFileSync(path.join(moduleDir, "src", "Widget.css"), ".widget {}\n");
  writeFileSync(
    path.join(moduleDir, "src", "sub", "Deep.tsx"),
    ['import "./Deep.css";', 'export const deep = "deep";', ""].join("\n"),
  );
  writeFileSync(path.join(moduleDir, "src", "sub", "Deep.css"), ".deep {}\n");
  writeFileSync(path.join(moduleDir, "src", "unused.css"), ".unused {}\n");

  try {
    await buildModule(moduleDir);

    expect(
      readFileSync(path.join(moduleDir, "dist", "Widget.js"), "utf8"),
    ).toContain('import "./Widget.css";');
    expect(
      readFileSync(path.join(moduleDir, "dist", "Widget.css"), "utf8"),
    ).toBe(".widget {}\n");
    expect(
      readFileSync(path.join(moduleDir, "dist", "sub", "Deep.js"), "utf8"),
    ).toContain('import "./Deep.css";');
    expect(
      readFileSync(path.join(moduleDir, "dist", "sub", "Deep.css"), "utf8"),
    ).toBe(".deep {}\n");
    expect(existsSync(path.join(moduleDir, "dist", "unused.css"))).toBe(false);
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

test("rejects CSS imports that resolve outside src/", async () => {
  const moduleDir = scaffoldModule();
  mkdirSync(path.join(moduleDir, "src"));
  writeFileSync(
    path.join(moduleDir, "src", "Widget.tsx"),
    ['import "../outside.css";', 'export const widget = "widget";', ""].join(
      "\n",
    ),
  );
  writeFileSync(path.join(moduleDir, "outside.css"), ".outside {}\n");

  try {
    await expect(buildModule(moduleDir)).rejects.toThrow(
      /Module CSS files must live inside src\//,
    );
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

test("rejects CSS imports of files that do not exist", async () => {
  const moduleDir = scaffoldModule();
  mkdirSync(path.join(moduleDir, "src"));
  writeFileSync(
    path.join(moduleDir, "src", "Widget.tsx"),
    ['import "./Missing.css";', 'export const widget = "widget";', ""].join(
      "\n",
    ),
  );

  try {
    await expect(buildModule(moduleDir)).rejects.toThrow(/does not exist/);
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

function scaffoldModule(): string {
  const moduleDir = mkdtempSync(
    path.join(tmpdir(), "wasp-module-builder-test-"),
  );
  writeFileSync(
    path.join(moduleDir, "package.json"),
    JSON.stringify({ name: "test-module", type: "module" }),
  );
  writeFileSync(
    path.join(moduleDir, "tsconfig.src.json"),
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "Bundler",
        strict: true,
        skipLibCheck: true,
        noEmit: true,
        lib: ["DOM", "ES2022"],
        jsx: "react-jsx",
      },
    }),
  );
  writeFileSync(
    path.join(moduleDir, "tsconfig.wasp.json"),
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "Bundler",
        strict: true,
        skipLibCheck: true,
        noEmit: true,
        lib: ["ES2022"],
        jsx: "preserve",
      },
    }),
  );
  writeModuleSpec(moduleDir, "string");

  return moduleDir;
}

function writeModuleSpec(moduleDir: string, prefixType: string): void {
  writeFileSync(
    path.join(moduleDir, "module.wasp.ts"),
    `export default function getModuleSpec(options: { prefix: ${prefixType} }) { return options; }\n`,
  );
}

function specDeclarationPath(moduleDir: string): string {
  return path.join(moduleDir, "dist", "spec.d.ts");
}

function specJavaScriptPath(moduleDir: string): string {
  return path.join(moduleDir, "dist", "spec.js");
}

function installSpecPackage(moduleDir: string): void {
  const scopeDir = path.join(moduleDir, "node_modules", "@wasp.sh");
  mkdirSync(scopeDir, { recursive: true });
  const specPackageDir = fileURLToPath(
    new URL("../node_modules/@wasp.sh/spec", import.meta.url),
  );
  symlinkSync(
    realpathSync(specPackageDir),
    path.join(scopeDir, "spec"),
    "junction",
  );
}

function expectDeclarationFilesResolve(moduleDir: string): void {
  const consumerPath = path.join(moduleDir, "consumer.ts");
  writeFileSync(
    consumerPath,
    [
      'import getModuleSpec from "./dist/spec.js";',
      'getModuleSpec({ prefix: "test", enabled: true });',
      "",
    ].join("\n"),
  );
  const program = ts.createProgram({
    rootNames: [consumerPath],
    options: {
      strict: true,
      noEmit: true,
      module: ts.ModuleKind.ESNext,
      moduleResolution: ts.ModuleResolutionKind.Bundler,
    },
  });
  const errors = ts
    .getPreEmitDiagnostics(program)
    .filter(
      (diagnostic) => diagnostic.category === ts.DiagnosticCategory.Error,
    );

  expect(
    errors.map((error) =>
      ts.flattenDiagnosticMessageText(error.messageText, "\n"),
    ),
  ).toEqual([]);
}
