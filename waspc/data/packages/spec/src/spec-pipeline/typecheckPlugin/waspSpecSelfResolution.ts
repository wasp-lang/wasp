import * as fs from "node:fs";
import { createRequire } from "node:module";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import ts from "typescript";

// `@wasp.sh/spec` and `@wasp.sh/spec/internal` -> this package's own `.d.ts`
// files, read from our `package.json` `exports` so the two never drift apart.
const OWN_SPEC_TYPE_ENTRY_POINTS = readOwnTypeEntryPoints(findOwnPackageDir());

/**
 * Makes the type checker see `@wasp.sh/spec` the way the runtime does (see
 * `ownSpecEntryPoints.ts`): as this package's own type declarations, never as
 * a copy found in the project's `node_modules`. It also gives the program a
 * last-resort `@types/node` for projects that don't have their dependencies
 * installed yet (e.g., a fresh clone).
 *
 * The compiler options stay untouched. Everything else is delegated to
 * TypeScript's own resolvers.
 */
export function resolveWaspSpecToSelfOnHost_mutate(
  host: ts.CompilerHost,
  compilerOptions: ts.CompilerOptions,
): void {
  const getCanonicalFileName = (fileName: string) =>
    host.getCanonicalFileName(fileName);
  const moduleResolutionCache = ts.createModuleResolutionCache(
    host.getCurrentDirectory(),
    getCanonicalFileName,
    compilerOptions,
  );

  // TypeScript expects a host that resolves modules itself to also expose its
  // resolution cache.
  host.getModuleResolutionCache = () => moduleResolutionCache;

  host.resolveModuleNameLiterals = (
    moduleLiterals,
    containingFile,
    redirectedReference,
    options,
    containingSourceFile,
  ) =>
    moduleLiterals.map((moduleLiteral) => {
      const ownTypesFile = OWN_SPEC_TYPE_ENTRY_POINTS.get(moduleLiteral.text);
      if (ownTypesFile !== undefined) {
        return {
          resolvedModule: {
            resolvedFileName: ownTypesFile,
            extension: ts.Extension.Dts,
            isExternalLibraryImport: true,
          },
        };
      }

      // The same mode TypeScript's default resolver computes: `import` vs
      // `require`, `with { "resolution-mode": ... }`, and the file's format.
      const mode = ts.getModeForUsageLocation(
        containingSourceFile,
        moduleLiteral,
        redirectedReference?.commandLine.options ?? options,
      );
      return ts.resolveModuleName(
        moduleLiteral.text,
        containingFile,
        options,
        host,
        moduleResolutionCache,
        redirectedReference,
        mode,
      );
    });

  // Taking over type reference resolution means taking it over for every
  // name, and TypeScript's default resolution mode for `/// <reference
  // types>` isn't public. So we only do it when TypeScript can't find
  // `@types/node` on its own. Projects with their dependencies installed keep
  // TypeScript's exact default behavior.
  if (canTypeScriptResolveNodeTypes(host, compilerOptions)) {
    return;
  }

  const typeReferenceResolutionCache =
    ts.createTypeReferenceDirectiveResolutionCache(
      host.getCurrentDirectory(),
      getCanonicalFileName,
      compilerOptions,
      moduleResolutionCache.getPackageJsonInfoCache(),
    );

  host.resolveTypeReferenceDirectiveReferences = (
    typeReferences,
    containingFile,
    redirectedReference,
    options,
    containingSourceFile,
  ) =>
    typeReferences.map((typeReference) => {
      const typeName =
        typeof typeReference === "string"
          ? typeReference
          : typeReference.fileName;
      // `impliedNodeFormat` is the closest public equivalent of TypeScript's
      // internal default (and what it passes to the older
      // `resolveTypeReferenceDirectives` hook).
      const mode = ts.getModeForFileReference(
        typeReference,
        containingSourceFile?.impliedNodeFormat,
      );

      const resolution = ts.resolveTypeReferenceDirective(
        typeName,
        containingFile,
        options,
        host,
        redirectedReference,
        typeReferenceResolutionCache,
        mode,
      );
      if (
        resolution.resolvedTypeReferenceDirective?.resolvedFileName ||
        typeName !== "node"
      ) {
        return resolution;
      }

      const fallbackTypeRoot = getOwnNodeTypesRoot();
      if (!fallbackTypeRoot) {
        return resolution;
      }

      // The options copy only lives for this one call. The program keeps
      // using the user's options.
      const fallbackResolution = ts.resolveTypeReferenceDirective(
        typeName,
        containingFile,
        { ...options, typeRoots: [fallbackTypeRoot] },
        host,
        redirectedReference,
        undefined,
        mode,
      );
      return fallbackResolution.resolvedTypeReferenceDirective?.resolvedFileName
        ? fallbackResolution
        : resolution;
    });
}

function canTypeScriptResolveNodeTypes(
  host: ts.CompilerHost,
  compilerOptions: ts.CompilerOptions,
): boolean {
  // The containing file TypeScript uses for `compilerOptions.types` entries.
  // The name matters: with custom `typeRoots`, TypeScript skips the
  // `node_modules` lookup only for this file. If TypeScript ever renamed it,
  // this check would find `@types/node` more often and skip the fallback,
  // which matches what `tsc` does.
  const configDir = compilerOptions.configFilePath
    ? path.dirname(compilerOptions.configFilePath as string)
    : host.getCurrentDirectory();
  const resolution = ts.resolveTypeReferenceDirective(
    "node",
    path.join(configDir, "__inferred type names__.ts"),
    compilerOptions,
    host,
  );
  return Boolean(resolution.resolvedTypeReferenceDirective?.resolvedFileName);
}

// `@types/node` is a dependency of this package, so Node's lookup finds it
// wherever this package's dependencies are installed.
function getOwnNodeTypesRoot(): string | undefined {
  try {
    const nodeTypesManifestPath = createRequire(import.meta.url).resolve(
      "@types/node/package.json",
    );
    return path.dirname(path.dirname(nodeTypesManifestPath));
  } catch {
    return undefined;
  }
}

function findOwnPackageDir(): string {
  for (
    let dir = path.dirname(fileURLToPath(import.meta.url));
    ;
    dir = path.dirname(dir)
  ) {
    const manifestPath = path.join(dir, "package.json");
    if (
      fs.existsSync(manifestPath) &&
      JSON.parse(fs.readFileSync(manifestPath, "utf8")).name === "@wasp.sh/spec"
    ) {
      return dir;
    }
    if (path.dirname(dir) === dir) {
      throw new Error("Could not find the @wasp.sh/spec package root.");
    }
  }
}

function readOwnTypeEntryPoints(packageDir: string): Map<string, string> {
  const manifest = JSON.parse(
    fs.readFileSync(path.join(packageDir, "package.json"), "utf8"),
  ) as { name: string; exports: Record<string, { types: string }> };

  return new Map(
    Object.entries(manifest.exports).map(([subpath, conditions]) => [
      path.posix.join(manifest.name, subpath),
      path.join(packageDir, conditions.types),
    ]),
  );
}
