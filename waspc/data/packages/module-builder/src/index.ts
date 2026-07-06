import type { RefObjectDescriptor } from "@wasp.sh/spec";
import { transformRefImports_mutate } from "@wasp.sh/spec/internal";
import assert from "node:assert/strict";
import { existsSync, readFileSync, realpathSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import type { RolldownMagicString } from "rolldown";
import type { ESTree as t } from "rolldown/utils";
import { build, type InlineConfig, type TsdownPlugin } from "tsdown";

type PackageJson = {
  name?: unknown;
};

async function main(): Promise<void> {
  const { moduleDir } = parseArgs(process.argv.slice(2));

  await buildModule(moduleDir);
}

export async function buildModule(moduleDir: string): Promise<void> {
  const packageName = readPackageName(moduleDir);
  const moduleSpecPath = path.join(moduleDir, "module.wasp.ts");

  if (!existsSync(moduleSpecPath)) {
    throw new Error(`Couldn't find module.wasp.ts in ${moduleDir}.`);
  }

  const commonOptions = {
    config: false,
    cwd: moduleDir,
    outDir: "dist",
    format: "esm",
    sourcemap: true,
    dts: { sourcemap: true },
    fixedExtension: false,
  } satisfies InlineConfig;

  await build({
    ...commonOptions,
    clean: true,
    entry: { spec: "./module.wasp.ts" },
    plugins: [lowerModuleRefImportsPlugin({ moduleDir, packageName })],
    platform: "node",
    target: "node22",
    deps: {
      neverBundle: ["@wasp.sh/spec", "@wasp.sh/spec/internal"],
    },
  });

  await build({
    ...commonOptions,
    clean: false,
    entry: { "*": ["src/**/*.ts", "src/**/*.tsx", "!src/**/*.d.ts"] },
    platform: "neutral",
    deps: {
      neverBundle: ["react", "react/jsx-runtime", "wasp/client/operations"],
    },
  });
}

function parseArgs(args: string[]): { moduleDir: string } {
  if (args.length !== 2 || args[0] !== "--module-dir") {
    throw new Error(
      "Usage: __internal_wasp_module_builder__ --module-dir <path>",
    );
  }

  return { moduleDir: realpathSync(path.resolve(args[1]!)) };
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
