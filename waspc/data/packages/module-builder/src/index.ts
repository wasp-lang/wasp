import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  realpathSync,
  statSync,
  writeFileSync,
} from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { parseAst } from "rolldown/parseAst";
import type { ESTree as t } from "rolldown/utils";
import { build } from "tsdown";
import ts from "typescript";

type PackageJson = {
  name?: unknown;
};

async function main(): Promise<void> {
  const { moduleDir, watch } = parseArgs(process.argv.slice(2));

  await buildModule(moduleDir, { watch });
}

export async function buildModule(
  moduleDir: string,
  { watch = false }: { watch?: boolean } = {},
): Promise<void> {
  // Validates that the module has a package.json with a name.
  readPackageName(moduleDir);

  const moduleSpecPath = path.join(moduleDir, "module.wasp.ts");
  if (!existsSync(moduleSpecPath)) {
    throw new Error(`Couldn't find module.wasp.ts in ${moduleDir}.`);
  }

  const moduleSpec = readFileSync(moduleSpecPath, "utf8");
  assertHasDefaultExport(moduleSpecPath, moduleSpec);

  const sourceEntries = getSourceEntries(moduleDir);
  if (Object.keys(sourceEntries).length > 0) {
    await build({
      config: false,
      cwd: moduleDir,
      outDir: "dist",
      format: "esm",
      sourcemap: true,
      dts: { sourcemap: true },
      fixedExtension: false,
      watch,
      clean: true,
      entry: sourceEntries,
      platform: "neutral",
      deps: {
        neverBundle: ["react", "react/jsx-runtime", /^wasp\//],
      },
    });
  }

  // The spec ships as source (module.wasp.ts), so only its declaration file is
  // built, for editor support in the host app.
  emitSpecDeclarationFile(moduleDir, moduleSpecPath);
}

function emitSpecDeclarationFile(
  moduleDir: string,
  moduleSpecPath: string,
): void {
  const program = ts.createProgram({
    rootNames: [moduleSpecPath],
    options: {
      declaration: true,
      emitDeclarationOnly: true,
      noCheck: true,
      skipLibCheck: true,
      target: ts.ScriptTarget.ES2022,
      module: ts.ModuleKind.ESNext,
      moduleResolution: ts.ModuleResolutionKind.Bundler,
      jsx: ts.JsxEmit.ReactJSX,
    },
  });

  let declarationFileText: string | undefined;
  program.emit(
    program.getSourceFile(moduleSpecPath),
    (_fileName, text) => {
      declarationFileText = text;
    },
    undefined,
    true,
  );

  if (declarationFileText === undefined) {
    throw new Error(`Couldn't emit a declaration file for ${moduleSpecPath}.`);
  }

  mkdirSync(path.join(moduleDir, "dist"), { recursive: true });
  writeFileSync(path.join(moduleDir, "dist", "spec.d.ts"), declarationFileText);
}

export function assertHasDefaultExport(
  moduleSpecPath: string,
  moduleSpec: string,
): void {
  const ast = parseAst(moduleSpec, { lang: "ts" });

  if (!hasDefaultExport(ast)) {
    throw new Error(
      `${moduleSpecPath} must default export a Wasp module spec function.`,
    );
  }
}

function hasDefaultExport(ast: t.Program): boolean {
  return ast.body.some((node) => node.type === "ExportDefaultDeclaration");
}

export function getSourceEntries(moduleDir: string): Record<string, string> {
  const sourceDir = path.join(moduleDir, "src");
  if (!existsSync(sourceDir)) {
    return {};
  }

  return listSourceEntryFiles(sourceDir).reduce<Record<string, string>>(
    (entries, sourceFilePath) => {
      const entryName = stripKnownExtension(
        path.relative(sourceDir, sourceFilePath),
      ).replaceAll(path.sep, "/");

      assert(
        entries[entryName] === undefined,
        `Duplicate module source entry ${JSON.stringify(entryName)}.`,
      );

      entries[entryName] = `./${path
        .relative(moduleDir, sourceFilePath)
        .replaceAll(path.sep, "/")}`;

      return entries;
    },
    {},
  );
}

function listSourceEntryFiles(sourceDir: string): string[] {
  return readdirSync(sourceDir)
    .flatMap((entryName) => {
      const entryPath = path.join(sourceDir, entryName);
      const entryStats = statSync(entryPath);

      if (entryStats.isDirectory()) {
        return listSourceEntryFiles(entryPath);
      }

      if (entryStats.isFile() && isSourceEntryFile(entryName)) {
        return [entryPath];
      }

      return [];
    })
    .sort();
}

function isSourceEntryFile(fileName: string): boolean {
  return (
    (fileName.endsWith(".ts") || fileName.endsWith(".tsx")) &&
    !fileName.endsWith(".d.ts")
  );
}

export function parseArgs(args: string[]): {
  moduleDir: string;
  watch: boolean;
} {
  if (
    (args.length !== 2 && args.length !== 3) ||
    args[0] !== "--module-dir" ||
    (args.length === 3 && args[2] !== "--watch")
  ) {
    throw new Error(
      "Usage: __internal_wasp_module_builder__ --module-dir <path> [--watch]",
    );
  }

  return {
    moduleDir: realpathSync(path.resolve(args[1]!)),
    watch: args[2] === "--watch",
  };
}

function readPackageName(moduleDir: string): string {
  const packageJsonPath = path.join(moduleDir, "package.json");

  if (!existsSync(packageJsonPath)) {
    throw new Error(`Couldn't find package.json in ${moduleDir}.`);
  }

  const packageJson = JSON.parse(
    readFileSync(packageJsonPath, "utf8"),
  ) as PackageJson;

  if (typeof packageJson.name !== "string" || packageJson.name.length === 0) {
    throw new Error(`${packageJsonPath} must define a non-empty string name.`);
  }

  return packageJson.name;
}

function stripKnownExtension(filePath: string): string {
  return filePath.replace(/\.(tsx?|jsx?)$/, "");
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? "").href) {
  main().catch((error: unknown) => {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  });
}
