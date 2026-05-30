import { fileURLToPath } from "node:url";
import type * as AppSpec from "../appSpec.js";
import { normalizeRefImportPath } from "./refImportPath.js";
import { SpecUserError } from "./specUserError.js";

/**
 * Reference to code in user's app's `src` directory.
 */
export type RefImport<T extends RefImportDescriptor = RefImportDescriptor> = T &
  RefImportMarker;

type RefImportMarker = { kind: "refImport" };

export type RefImportDescriptor =
  | NamedRefImportDescriptor
  | DefaultRefImportDescriptor;

export interface NamedRefImportDescriptor {
  import: string;
  alias?: string;
  from: string;
}

export interface DefaultRefImportDescriptor {
  importDefault: string;
  from: string;
}

export function ref<T extends RefImportDescriptor>(
  descriptor: T,
): RefImport<T> {
  return { ...descriptor, kind: "refImport" };
}

/**
 * Creates a `ref` helper bound to the user's `.wasp.ts` file.
 *
 * Ref imports need the current spec file location to resolve relative paths,
 * but `ref` itself can't use `import.meta.url` because it would point to
 * this helper module. `_waspMakeRef(import.meta.url)` lets each `.wasp.ts`
 * file create a local `ref` that carries its own source file path.
 *
 * @internal
 */
export function _waspMakeRef(
  importingFileUrl: string,
): <T extends RefImportDescriptor>(descriptor: T) => RefImport<T> {
  const sourceFilePath = fileURLToPath(importingFileUrl);

  return (descriptor) => ({ ...ref(descriptor), sourceFilePath });
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
      `Relative ref path ${JSON.stringify(refImport.from)} is missing source file information. Use ref(...) in a *.wasp.ts file.`,
    );
  }

  return normalizeRefImportPath({
    importPath: refImport.from,
    importingFilePath: refImport.sourceFilePath,
    projectRootDir,
  });
}

type RefImportSource = {
  sourceFilePath: string;
};

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
