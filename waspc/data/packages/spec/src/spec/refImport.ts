import { fileURLToPath } from "node:url";
import type * as AppSpec from "../appSpec.js";
import { normalizeRefImportPath } from "./refImportPath.js";
import { SpecUserError } from "./specUserError.js";

/**
 * A reference to code in your app's `src` directory.
 *
 * Use this when you can't use a reference import.
 * The import path must start with `@src/` and be either a single named import
 * ({@link NamedExtImport}) or a default import ({@link DefaultExtImport}).
 */
export type ExtImport = NamedExtImport | DefaultExtImport;

/**
 * User-facing reference to code in your app's `src` directory.
 */
export type RefImport<T extends RefImportDescriptor = RefImportDescriptor> =
  Omit<T, "kind"> & { kind: "refImport" };

export type RefImportDescriptor =
  | NamedRefImportDescriptor
  | DefaultRefImportDescriptor;

export type RefImportSource = {
  sourceFilePath: string;
};

export type RefImportWithSourcePath<
  T extends RefImportDescriptor = RefImportDescriptor,
> = RefImport<T & RefImportSource>;

export type RefImportHelper = <T extends RefImportDescriptor>(
  descriptor: T,
) => RefImport<T>;

export type SourceAwareRefImportHelper = <T extends RefImportDescriptor>(
  descriptor: T,
) => RefImportWithSourcePath<T>;

export interface NamedRefImportDescriptor {
  import: string;
  alias?: string;
  from: string;
}

export interface DefaultRefImportDescriptor {
  importDefault: string;
  from: string;
}

export function refImport<T extends RefImportDescriptor>(
  descriptor: T,
): RefImport<T> {
  return { ...descriptor, kind: "refImport" } as RefImport<T>;
}

export function makeRefImport(
  importingFileUrl: string,
): SourceAwareRefImportHelper {
  const sourceFilePath = fileURLToPath(importingFileUrl);

  return (descriptor) =>
    refImport({ ...descriptor, sourceFilePath }) as RefImportWithSourcePath<
      typeof descriptor
    >;
}

/**
 * Named import reference, equivalent to
 * `import { SomeValue } from "./src/someModule" with { type: "ref" }`.
 */
export interface NamedExtImport {
  /** Exported name to import. */
  import: string;
  /**
   * Optional local alias.
   *
   * When Wasp derives a declaration name from this import, the alias takes
   * precedence over the `import` field.
   */
  alias?: string;
  /** Module path. Must start with `@src/`. */
  from: AppSpec.ExtImport["path"];
}

/**
 * Default import reference, equivalent to
 * `import SomeValue from "./src/someModule" with { type: "ref" }`.
 */
export interface DefaultExtImport {
  /** Local name for the default import. */
  importDefault: string;
  /** Module path. Must start with `@src/`. */
  from: AppSpec.ExtImport["path"];
}

export function mapRefImportToExtImport(
  refImport: unknown,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImport {
  if (isNamedRefImportDescriptor(refImport)) {
    return {
      kind: "named",
      name: refImport.import,
      path: mapRefImportPath(refImport, { projectRootDir }),
      alias: refImport.alias,
    };
  } else if (isDefaultRefImportDescriptor(refImport)) {
    return {
      kind: "default",
      name: refImport.importDefault,
      path: mapRefImportPath(refImport, { projectRootDir }),
    };
  } else {
    throw new SpecUserError(
      "Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(refImport) +
        '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the RefImport object correctly.',
    );
  }
}

export function getRefImportDeclarationName(refImport: unknown): string {
  if (isNamedRefImportDescriptor(refImport)) {
    return refImport.alias ?? refImport.import;
  }

  if (isDefaultRefImportDescriptor(refImport)) {
    return refImport.importDefault;
  }

  throw new SpecUserError(
    "Got an import in the Wasp file that we couldn't process: " +
      JSON.stringify(refImport),
  );
}

function mapRefImportPath(
  refImport: RefImportDescriptor,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImport["path"] {
  if (isAppSpecExtImportPath(refImport.from)) {
    // TODO: Remove raw @src descriptor support after reference import lowering
    // emits refImport(...) calls instead of plain descriptors.
    return refImport.from;
  }

  if (!hasSourceFilePath(refImport)) {
    throw new SpecUserError(
      `Relative refImport path ${JSON.stringify(refImport.from)} is missing source file information. Use refImport(...) in a *.wasp.ts file.`,
    );
  }

  return normalizeRefImportPath({
    importPath: refImport.from,
    importingFilePath: refImport.sourceFilePath,
    projectRootDir,
  });
}

function isAppSpecExtImportPath(
  path: string,
): path is AppSpec.ExtImport["path"] {
  return path.startsWith("@src/");
}

function hasSourceFilePath(value: unknown): value is RefImportSource {
  return (
    isObject(value) &&
    "sourceFilePath" in value &&
    typeof value.sourceFilePath === "string"
  );
}

function isNamedRefImportDescriptor(
  value: unknown,
): value is NamedRefImportDescriptor {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value) &&
    isSupportedRefImportDescriptor(value.from, value.kind)
  );
}

function isDefaultRefImportDescriptor(
  value: unknown,
): value is DefaultRefImportDescriptor {
  return (
    isObject(value) &&
    typeof value.importDefault === "string" &&
    typeof value.from === "string" &&
    isSupportedRefImportDescriptor(value.from, value.kind)
  );
}

function isSupportedRefImportDescriptor(from: string, kind: unknown): boolean {
  return kind === "refImport" || isAppSpecExtImportPath(from);
}

// TODO: Remove after reference import lowering emits refImport(...) calls instead
// of plain descriptors.
export function isNamedExtImport(value: unknown): value is NamedExtImport {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value)
  );
}

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
