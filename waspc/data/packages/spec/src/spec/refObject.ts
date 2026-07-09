import type * as AppSpec from "../appSpec.js";
import type { Branded } from "../branded.js";
import type * as WaspSpec from "./publicApi/waspSpec.js";
import {
  normalizeRefObjectPath,
  tryMapPackageRefObjectPath,
} from "./refObjectPath.js";
import { WaspSpecUserError } from "./waspSpecUserError.js";

/**
 * A reference to code used by a Wasp app or module.
 *
 * @category References
 */
export type RefObject = Branded<
  RefObjectDescriptor & { kind: "refObject" },
  "RefObject"
>;

/**
 * Input accepted by {@link ref}.
 *
 * @category References
 */
export type RefObjectDescriptor =
  | NamedRefObjectDescriptor
  | DefaultRefObjectDescriptor;

/**
 * Named import reference, equivalent to
 * `import { SomeValue } from "./src/someModule" with { type: "ref" }`.
 *
 * @category References
 */
export interface NamedRefObjectDescriptor {
  /** Exported name to import. */
  import: string;
  /**
   * Optional local alias.
   *
   * Alias takes precedence over the `import` field when
   * Wasp Spec derives some {@link WaspSpec.SpecElement} name.
   */
  alias?: string;
  /** Relative app path or package specifier. */
  from: string;
}

/**
 * Default import reference, equivalent to
 * `import SomeValue from "./src/someModule" with { type: "ref" }`.
 *
 * @category References
 */
export interface DefaultRefObjectDescriptor {
  /** Local name for the default import. */
  importDefault: string;
  /** Relative app path or package specifier. */
  from: string;
}

/**
 * Creates a fallback reference object for a value from your app or module.
 *
 * {@include ./publicApi/referenceImports.md}
 *
 * Reference imports are preferred because editors can follow and rename real
 * imports.
 *
 * Relative import paths must resolve inside the app's `src/` directory.
 * Non-relative import paths are treated as package imports.
 * Absolute paths are not supported.
 *
 * @category References
 *
 * @example
 * ```ts
 * import { page, ref } from "@wasp.sh/spec"
 *
 * const MainPage = ref({
 *   importDefault: "MainPage",
 *   from: "./src/MainPage",
 * })
 *
 * export const mainPage = page(MainPage)
 * ```
 */
export function ref(_descriptor: RefObjectDescriptor): RefObject {
  throw new Error(
    "Missing Wasp transformation. The `.wasp.ts` files are not directly executable, use the Wasp CLI.",
  );
}

/**
 * Creates a `ref` helper bound to the user's `.wasp.ts` file.
 *
 * Ref objects need the current spec file location to resolve relative paths,
 * but `ref` itself can't use `import.meta.url` because it would point to this
 * helper module. `_waspMakeRef(sourceFilePath)` lets each `.wasp.ts` file
 * create a local `ref` that carries its own source file path.
 *
 * @internal
 */
export function _waspMakeRef(
  sourceFilePath: string,
): (descriptor: RefObjectDescriptor) => SourceAwareRefObject {
  return (descriptor: RefObjectDescriptor) => {
    const refObject = {
      ...descriptor,
      kind: "refObject",
    } as RefObject;

    return { ...refObject, sourceFilePath };
  };
}

export function mapRefObject(
  refObject: unknown,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImport {
  if (isNamedRefObject(refObject)) {
    return {
      kind: "named",
      name: refObject.import,
      source: mapRefObjectSource(refObject, { projectRootDir }),
      alias: refObject.alias,
    };
  } else if (isDefaultRefObject(refObject)) {
    return {
      kind: "default",
      name: refObject.importDefault,
      source: mapRefObjectSource(refObject, { projectRootDir }),
    };
  } else {
    throw new WaspSpecUserError(
      "Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(refObject) +
        '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the ref object correctly.',
    );
  }
}

export function getRefObjectDeclarationName(refObject: unknown): string {
  if (isNamedRefObject(refObject)) {
    return refObject.alias ?? refObject.import;
  }

  if (isDefaultRefObject(refObject)) {
    return refObject.importDefault;
  }

  throw new WaspSpecUserError(
    "Got an import in the Wasp file that we couldn't process: " +
      JSON.stringify(refObject),
  );
}

function mapRefObjectSource(
  refObject: RefObjectDescriptor,
  { projectRootDir }: { projectRootDir: string },
): AppSpec.ExtImportSource {
  if (isAbsoluteRefPath(refObject.from)) {
    throw new WaspSpecUserError(
      `Absolute ref paths are not supported: ${JSON.stringify(refObject.from)}. Use a relative path or a package import.`,
    );
  }

  if (!refObject.from.startsWith(".")) {
    return {
      kind: "package",
      ...splitPackageSpecifier(refObject.from),
    };
  }

  if (!hasSourceFilePath(refObject)) {
    throw new WaspSpecUserError(
      `Relative ref path ${JSON.stringify(refObject.from)} is missing source file information. Use \`ref(...)\` in a \`*.wasp.ts\` file.`,
    );
  }

  const packageSource = tryMapPackageRefObjectPath({
    importPath: refObject.from,
    importingFilePath: refObject.sourceFilePath,
    projectRootDir,
  });
  if (packageSource !== undefined) {
    return packageSource;
  }

  return {
    kind: "project-src",
    path: normalizeRefObjectPath({
      importPath: refObject.from,
      importingFilePath: refObject.sourceFilePath,
      projectRootDir,
    }),
  };
}

function splitPackageSpecifier(
  specifier: string,
): Omit<AppSpec.PackageExtImportSource, "kind"> {
  const [firstPart, secondPart, ...remainingParts] = specifier.split("/");

  if (specifier.startsWith("@")) {
    if (!secondPart) {
      throw new WaspSpecUserError(
        `Scoped package ref ${JSON.stringify(specifier)} must include both scope and package name.`,
      );
    }

    return {
      packageName: `${firstPart}/${secondPart}`,
      subpath: remainingParts.join("/") || undefined,
    };
  }

  return {
    packageName: firstPart ?? "",
    subpath:
      [secondPart, ...remainingParts].filter(Boolean).join("/") || undefined,
  };
}

function isAbsoluteRefPath(path: string): boolean {
  return (
    path.startsWith("/") ||
    /^[A-Za-z]:[\\/]/.test(path) ||
    path.startsWith("\\\\")
  );
}

type RefObjectSource = {
  sourceFilePath: string;
};

type SourceAwareRefObject = RefObject & RefObjectSource;

function hasSourceFilePath(value: unknown): value is RefObjectSource {
  return (
    isObject(value) &&
    "sourceFilePath" in value &&
    typeof value.sourceFilePath === "string"
  );
}

function isNamedRefObject(value: unknown): value is NamedRefObjectDescriptor {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value) &&
    hasRefObjectMarker(value)
  );
}

function isDefaultRefObject(
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
