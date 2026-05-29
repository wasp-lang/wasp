import { fileURLToPath } from "node:url";
import type * as AppSpec from "../appSpec.js";
import { normalizeRefImportPath } from "./refImportPath.js";
import { SpecUserError } from "./specUserError.js";

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

/**
 * Creates a `refImport` helper bound to the user's `.wasp.ts` file.
 *
 * Ref imports need the current spec file location to resolve relative paths,
 * but `refImport` itself can't use `import.meta.url` because it would point to
 * this helper module. `makeRefImport(import.meta.url)` lets each `.wasp.ts`
 * file create a local `refImport` that carries its own source file path.
 */
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
 * Converts a public `RefImport` into the internal `AppSpec.ExtImport` shape.
 *
 * This is the only place relative reference paths are normalized, keeping the
 * TypeScript API source-relative while preserving the `@src/...` shape expected
 * by the Haskell compiler.
 *
 * For example, `{ importDefault: "MainPage", from: "./src/MainPage" }` from
 * `/app/main.wasp.ts` becomes `{ kind: "default", name: "MainPage", path:
 * "@src/MainPage" }`.
 */
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
    hasRefImportMarker(value)
  );
}

function isDefaultRefImportDescriptor(
  value: unknown,
): value is DefaultRefImportDescriptor {
  return (
    isObject(value) &&
    typeof value.importDefault === "string" &&
    typeof value.from === "string" &&
    hasRefImportMarker(value)
  );
}

function hasRefImportMarker(value: Record<string, unknown>): boolean {
  return value.kind === "refImport";
}

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
