import type { RefObjectDescriptor } from "@wasp.sh/spec";
import { transformRefImports_mutate } from "@wasp.sh/spec/internal";
import assert from "node:assert/strict";
import {
  existsSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import type { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import type { ESTree as t } from "rolldown/utils";
import { build, type InlineConfig, type TsdownPlugin } from "tsdown";

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
  const packageName = readPackageName(moduleDir);
  const moduleSpecPath = path.join(moduleDir, "module.wasp.ts");

  if (!existsSync(moduleSpecPath)) {
    throw new Error(`Couldn't find module.wasp.ts in ${moduleDir}.`);
  }

  const moduleSpec = readFileSync(moduleSpecPath, "utf8");
  assertHasDefaultExport(moduleSpecPath, moduleSpec);

  const commonOptions = {
    config: false,
    cwd: moduleDir,
    outDir: "dist",
    format: "esm",
    sourcemap: true,
    dts: { sourcemap: true },
    fixedExtension: false,
    watch,
  } satisfies InlineConfig;

  await build({
    ...commonOptions,
    clean: true,
    entry: { spec: "./module.wasp.ts" },
    plugins: [lowerModuleRefImportsPlugin({ moduleDir, packageName })],
    platform: "node",
    target: "node22",
    deps: {
      alwaysBundle: [/.*/],
    },
    hooks: {
      "build:done": () => {
        writeLooseModuleSpecTypes(moduleDir, moduleSpec);
      },
    },
  });

  const sourceEntries = getSourceEntries(moduleDir);
  if (Object.keys(sourceEntries).length > 0) {
    await build({
      ...commonOptions,
      clean: false,
      entry: sourceEntries,
      platform: "neutral",
      deps: {
        neverBundle: ["react", "react/jsx-runtime", /^wasp\//],
      },
    });
  }
}

function writeLooseModuleSpecTypes(
  moduleDir: string,
  moduleSpec: string,
): void {
  writeFileSync(
    path.join(moduleDir, "dist", "spec.d.ts"),
    getLooseModuleSpecTypes(moduleSpec),
  );
  rmSync(path.join(moduleDir, "dist", "spec.d.ts.map"), { force: true });
}

export function getLooseModuleSpecTypes(moduleSpec: string): string {
  const exportedNames = [
    ...getDirectExportNames(moduleSpec),
    ...getNamedExportNames(moduleSpec),
  ];
  const uniqueExportedNames = [...new Set(exportedNames)].sort();
  const hasDefaultExport =
    /export\s+default\b/.test(moduleSpec) ||
    /export\s+(?!type\b)\{[^}]+\bas\s+default\b[^}]*\}/.test(moduleSpec);
  const typeLines = [
    ...uniqueExportedNames.map(
      (exportedName) => `declare const ${exportedName}: any;`,
    ),
    hasDefaultExport ? "declare const _default: any;" : null,
    uniqueExportedNames.length > 0
      ? `export { ${uniqueExportedNames.join(", ")} };`
      : null,
    hasDefaultExport ? "export default _default;" : null,
  ].filter((line) => line !== null);

  return `${typeLines.join("\n")}\n`;
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

function getDirectExportNames(moduleSpec: string): string[] {
  return [
    ...moduleSpec.matchAll(
      /export\s+(?:const|let|var|function|class)\s+([A-Za-z_$][A-Za-z0-9_$]*)/g,
    ),
  ].map((match) => match[1]!);
}

function getNamedExportNames(moduleSpec: string): string[] {
  return [...moduleSpec.matchAll(/export\s+(?!type\b)\{([^}]+)\}/g)].flatMap(
    (match) => {
      return match[1]!
        .split(",")
        .map((specifier) => specifier.trim())
        .filter((specifier) => specifier.length > 0)
        .map((specifier) =>
          specifier
            .split(/\s+as\s+/)
            .at(-1)!
            .trim(),
        )
        .filter((exportedName) => exportedName !== "default");
    },
  );
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

function lowerModuleRefImportsPlugin({
  moduleDir,
  packageName,
}: {
  moduleDir: string;
  packageName: string;
}): TsdownPlugin {
  const moduleSpecPath = path.resolve(moduleDir, "module.wasp.ts");

  return {
    name: "wasp/module-builder/lower-ref-imports",
    transform: {
      filter: { id: /module\.wasp\.ts$/ },
      handler(code, id, meta) {
        if (path.resolve(id) !== moduleSpecPath) {
          return null;
        }

        assert(meta.magicString);

        const ast = meta.ast || this.parse(code, { lang: "ts" });
        transformModuleRefImports_mutate(ast, meta.magicString, {
          moduleDir,
          packageName,
        });

        return { code: meta.magicString };
      },
    },
  };
}

export function transformModuleRefImports_mutate(
  ast: t.Program,
  magicString: RolldownMagicString,
  { moduleDir, packageName }: { moduleDir: string; packageName: string },
): void {
  transformRefImports_mutate(ast, magicString, {
    extraSafeNameBases: {
      fileURLToPath: "fileURLToPath",
      makeRef: "_waspMakeRef",
    },
    getRefHelperPrelude: ({ safeRefHelperName, extraSafeNames }) => {
      const fileURLToPathName = extraSafeNames.fileURLToPath!;
      const makeRefName = extraSafeNames.makeRef!;

      return [
        buildNamedImport("fileURLToPath", fileURLToPathName, "node:url"),
        buildNamedImport("_waspMakeRef", makeRefName, "@wasp.sh/spec/internal"),
        `const ${safeRefHelperName} = ${makeRefName}(${fileURLToPathName}(import.meta.url));\n`,
      ].join("");
    },
    mapRefObjectDescriptor: (descriptor) =>
      mapRefObjectDescriptorToPackageSource(descriptor, {
        moduleDir,
        packageName,
      }),
  });
}

function buildNamedImport(
  importName: string,
  localName: string,
  source: string,
) {
  const specifier =
    importName === localName ? importName : `${importName} as ${localName}`;

  return `import { ${specifier} } from ${JSON.stringify(source)};\n`;
}

function mapRefObjectDescriptorToPackageSource(
  descriptor: RefObjectDescriptor,
  { moduleDir, packageName }: { moduleDir: string; packageName: string },
): RefObjectDescriptor {
  return {
    ...descriptor,
    from: getModulePackageRefSource({
      from: descriptor.from,
      moduleDir,
      packageName,
    }),
  };
}

export function getModulePackageRefSource({
  from,
  moduleDir,
  packageName,
}: {
  from: string;
  moduleDir: string;
  packageName: string;
}): string {
  if (!from.startsWith(".")) {
    return from;
  }

  const sourceFilePath = path.resolve(moduleDir, from);
  const sourceDir = path.resolve(moduleDir, "src");
  const relativeSourcePath = path.relative(sourceDir, sourceFilePath);

  if (
    relativeSourcePath.startsWith("..") ||
    path.isAbsolute(relativeSourcePath)
  ) {
    throw new Error(
      `Relative module ref ${JSON.stringify(from)} must resolve inside src/.`,
    );
  }

  const packageSubpath = stripKnownExtension(relativeSourcePath).replaceAll(
    path.sep,
    "/",
  );

  return `${packageName}/${packageSubpath}`;
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
