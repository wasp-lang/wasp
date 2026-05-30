import { fileURLToPath } from "node:url";
import type * as AppSpec from "../appSpec.js";
import { normalizeRefObjectPath } from "./refObjectPath.js";
import { SpecUserError } from "./specUserError.js";

/**
 * A reference to code in your app's `src` directory.
 */
export type RefObject<T extends RefObjectDescriptor = RefObjectDescriptor> = T &
  RefObjectMarker;

type RefObjectMarker = { kind: "refObject" };

export type RefObjectDescriptor =
  | NamedRefObjectDescriptor
  | DefaultRefObjectDescriptor;

/**
 * Named import reference, equivalent to
 * `import { SomeValue } from "./src/someModule" with { type: "ref" }`.
 */
export interface NamedRefObjectDescriptor {
  /** Exported name to import. */
  import: string;
  /**
   * Optional local alias.
   *
   * When Wasp derives a declaration name from this import, the alias takes
   * precedence over the `import` field.
   */
  alias?: string;
  /** Module path, relative to the `*.wasp.ts` file using it. */
  from: string;
}

/**
 * Default import reference, equivalent to
 * `import SomeValue from "./src/someModule" with { type: "ref" }`.
 */
export interface DefaultRefObjectDescriptor {
  /** Local name for the default import. */
  importDefault: string;
  /** Module path, relative to the `*.wasp.ts` file using it. */
  from: string;
}

export function ref<T extends RefObjectDescriptor>(
  descriptor: T,
): RefObject<T> {
  return { ...descriptor, kind: "refObject" };
}

/**
 * Creates a `ref` helper bound to the user's `.wasp.ts` file.
 *
 * Ref objects need the current spec file location to resolve relative paths,
 * but `ref` itself can't use `import.meta.url` because it would point to this
 * helper module. `_waspMakeRef(import.meta.url)` lets each `.wasp.ts` file
 * create a local `ref` that carries its own source file path.
 *
 * @internal
 */
export function _waspMakeRef(
  importingFileUrl: string,
): <T extends RefObjectDescriptor>(descriptor: T) => RefObject<T> {
  const sourceFilePath = fileURLToPath(importingFileUrl);

  return (descriptor) => ({ ...ref(descriptor), sourceFilePath });
}

export function mapRefObject(
  refObject: unknown,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImport {
  if (isNamedRefObjectDescriptor(refObject)) {
    return {
      kind: "named",
      name: refObject.import,
      path: mapRefObjectPath(refObject, { projectRootDir }),
      alias: refObject.alias,
    };
  } else if (isDefaultRefObjectDescriptor(refObject)) {
    return {
      kind: "default",
      name: refObject.importDefault,
      path: mapRefObjectPath(refObject, { projectRootDir }),
    };
  } else {
    throw new SpecUserError(
      "Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(refObject) +
        '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the ref object correctly.',
    );
  }
}

export function getRefObjectDeclarationName(refObject: unknown): string {
  if (isNamedRefObjectDescriptor(refObject)) {
    return refObject.alias ?? refObject.import;
  }

  if (isDefaultRefObjectDescriptor(refObject)) {
    return refObject.importDefault;
  }

  throw new SpecUserError(
    "Got an import in the Wasp file that we couldn't process: " +
      JSON.stringify(refObject),
  );
}

function mapRefObjectPath(
  refObject: RefObjectDescriptor,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImport["path"] {
  if (!hasSourceFilePath(refObject)) {
    throw new SpecUserError(
      `Relative ref path ${JSON.stringify(refObject.from)} is missing source file information. Use ref(...) in a *.wasp.ts file.`,
    );
  }

  return normalizeRefObjectPath({
    importPath: refObject.from,
    importingFilePath: refObject.sourceFilePath,
    projectRootDir,
  });
}

type RefObjectSource = {
  sourceFilePath: string;
};

function hasSourceFilePath(value: unknown): value is RefObjectSource {
  return (
    isObject(value) &&
    "sourceFilePath" in value &&
    typeof value.sourceFilePath === "string"
  );
}

function isNamedRefObjectDescriptor(
  value: unknown,
): value is NamedRefObjectDescriptor {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value) &&
    hasRefObjectMarker(value)
  );
}

function isDefaultRefObjectDescriptor(
  value: unknown,
): value is DefaultRefObjectDescriptor {
  return (
    isObject(value) &&
    typeof value.importDefault === "string" &&
    typeof value.from === "string" &&
    hasRefObjectMarker(value)
  );
}

function hasRefObjectMarker(value: Record<string, unknown>): boolean {
  return value.kind === "refObject";
}

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
