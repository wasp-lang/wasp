import { createWaspTsSpecPlugins } from "@wasp.sh/spec/compiler";
import assert from "node:assert/strict";
import {
  existsSync,
  readdirSync,
  readFileSync,
  realpathSync,
  statSync,
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
  const { moduleDir } = parseArgs(process.argv.slice(2));

  await buildModule(moduleDir);
}

export async function buildModule(moduleDir: string): Promise<void> {
  moduleDir = realpathSync(moduleDir);
  const packageName = readPackageName(moduleDir);

  const moduleSpecPath = path.join(moduleDir, "module.wasp.ts");
  if (!existsSync(moduleSpecPath)) {
    throw new Error(`Couldn't find module.wasp.ts in ${moduleDir}.`);
  }

  const moduleSpec = readFileSync(moduleSpecPath, "utf8");
  assertHasDefaultExport(moduleSpecPath, moduleSpec);
  typecheckModuleSpec(moduleDir);

  await build({
    config: false,
    cwd: moduleDir,
    outDir: "dist",
    format: "esm",
    sourcemap: false,
    dts: { sourcemap: false },
    fixedExtension: false,
    clean: true,
    entry: { spec: "./module.wasp.ts" },
    platform: "node",
    target: "node24",
    tsconfig: "tsconfig.wasp.json",
    plugins: createWaspTsSpecPlugins({
      tsconfigPath: path.join(moduleDir, "tsconfig.wasp.json"),
      getRefOrigin: (filePath) => ({
        kind: "package",
        packageName,
        specFilePath: getModuleRelativePath(moduleDir, filePath),
      }),
    }),
    deps: {
      neverBundle: [/^[^./]/],
    },
  });

  const sourceEntries = getSourceEntries(moduleDir);
  if (Object.keys(sourceEntries).length > 0) {
    await build({
      config: false,
      cwd: moduleDir,
      outDir: "dist",
      format: "esm",
      sourcemap: true,
      dts: true,
      fixedExtension: false,
      clean: false,
      entry: sourceEntries,
      platform: "neutral",
      // The root tsconfig.json is a solution file with references, which the
      // dts plugin can't consume. Source code compiles under tsconfig.src.json.
      tsconfig: "tsconfig.src.json",
      deps: {
        neverBundle: ["react", "react/jsx-runtime", /^wasp\//],
      },
    });
  }
}

function typecheckModuleSpec(moduleDir: string): void {
  const waspTsConfigPath = path.join(moduleDir, "tsconfig.wasp.json");
  const configFile = ts.readConfigFile(waspTsConfigPath, ts.sys.readFile);
  if (configFile.error) {
    throw new Error(
      ts.formatDiagnosticsWithColorAndContext([configFile.error], {
        getCanonicalFileName: (fileName) => fileName,
        getCurrentDirectory: () => moduleDir,
        getNewLine: () => ts.sys.newLine,
      }),
    );
  }

  const parsed = ts.parseJsonConfigFileContent(
    configFile.config,
    ts.sys,
    moduleDir,
    undefined,
    waspTsConfigPath,
  );
  if (parsed.errors.length > 0) {
    throw new Error(
      ts.formatDiagnosticsWithColorAndContext(parsed.errors, {
        getCanonicalFileName: (fileName) => fileName,
        getCurrentDirectory: () => moduleDir,
        getNewLine: () => ts.sys.newLine,
      }),
    );
  }

  const program = ts.createProgram({
    rootNames: parsed.fileNames,
    options: parsed.options,
  });
  const errors = ts
    .getPreEmitDiagnostics(program)
    .filter(
      (diagnostic) =>
        diagnostic.category === ts.DiagnosticCategory.Error &&
        (diagnostic.file === undefined ||
          diagnostic.file.fileName.endsWith(".wasp.ts")),
    );

  if (errors.length > 0) {
    throw new Error(
      ts.formatDiagnosticsWithColorAndContext(errors, {
        getCanonicalFileName: (fileName) => fileName,
        getCurrentDirectory: () => moduleDir,
        getNewLine: () => ts.sys.newLine,
      }),
    );
  }
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
        entryName.toLowerCase() !== "spec",
        'Module source entry "spec" conflicts with the compiled module spec.',
      );
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

function getModuleRelativePath(moduleDir: string, filePath: string): string {
  const relativePath = tryGetModuleRelativePath(moduleDir, filePath);
  if (relativePath === undefined) {
    throw new Error(
      `Module spec file ${JSON.stringify(filePath)} must be inside ${JSON.stringify(moduleDir)}.`,
    );
  }

  return relativePath;
}

function tryGetModuleRelativePath(
  moduleDir: string,
  filePath: string,
): string | undefined {
  const relativePath = path.relative(moduleDir, filePath);
  if (
    path.isAbsolute(relativePath) ||
    relativePath === ".." ||
    relativePath.startsWith(`..${path.sep}`)
  ) {
    return undefined;
  }

  return relativePath.replaceAll(path.sep, "/");
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
    !fileName.endsWith(".d.ts") &&
    !fileName.endsWith(".wasp.ts")
  );
}

export function parseArgs(args: string[]): {
  moduleDir: string;
} {
  if (args.length !== 2 || args[0] !== "--module-dir") {
    throw new Error(
      "Usage: __internal_wasp_module_builder__ --module-dir <path>",
    );
  }

  return {
    moduleDir: realpathSync(path.resolve(args[1]!)),
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
