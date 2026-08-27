import { existsSync, readFileSync, realpathSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { parseAst } from "rolldown/parseAst";
import type { ESTree as t } from "rolldown/utils";
import { buildWithConfigs, resolveUserConfig } from "tsdown";
import ts from "typescript";
import { getModuleBuildConfigs } from "./moduleBuildConfigs.js";

async function main(): Promise<void> {
  const { moduleDir } = parseArgs(process.argv.slice(2));

  await buildModule(moduleDir);
}

export async function buildModule(moduleDir: string): Promise<void> {
  moduleDir = realpathSync(moduleDir);

  const moduleSpecPath = path.join(moduleDir, "module.wasp.ts");
  if (!existsSync(moduleSpecPath)) {
    throw new Error(`Couldn't find module.wasp.ts in ${moduleDir}.`);
  }

  const moduleSpec = readFileSync(moduleSpecPath, "utf8");
  assertHasDefaultExport(moduleSpecPath, moduleSpec);
  typecheckModuleSpec(moduleDir);

  const configDependencies = new Set<string>();
  const inlineConfig = { cwd: moduleDir, config: false };
  const configs = (
    await Promise.all(
      getModuleBuildConfigs(moduleDir).map((config) =>
        resolveUserConfig(config, inlineConfig, configDependencies),
      ),
    )
  ).flat();

  // tsdown's public build API accepts only one inline config. These internal
  // APIs preserve its coordinated cleaning and package-export generation.
  await buildWithConfigs(configs, configDependencies, () => undefined);
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

if (import.meta.url === pathToFileURL(process.argv[1] ?? "").href) {
  main().catch((error: unknown) => {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  });
}
