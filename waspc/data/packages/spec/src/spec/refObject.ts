import { posix as posixPath, win32 as windowsPath } from "node:path";
import type * as AppSpec from "../appSpec.js";
import type { Branded } from "../branded.js";
import type * as WaspSpec from "./publicApi/waspSpec.js";
import { normalizeRefObjectPath } from "./refObjectPath.js";
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

export type RefOrigin = ProjectRefOrigin | PackageRefOrigin;

export interface ProjectRefOrigin {
  kind: "project";
  /** Path to the spec file, relative to the project root. */
  specFilePath: string;
}

export interface PackageRefOrigin {
  kind: "package";
  packageName: string;
  /** Path to the spec file, relative to the package root. */
  specFilePath: string;
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
 * Creates a `ref` helper bound to the logical origin of a `.wasp.ts` file.
 *
 * Ref objects need their spec file's project- or package-relative path to
 * resolve relative imports without depending on a machine's filesystem.
 *
 * @internal
 */
export function _waspMakeRef(
  origin: RefOrigin,
): (descriptor: RefObjectDescriptor) => OriginAwareRefObject {
  return (descriptor: RefObjectDescriptor) => {
    const refObject = {
      ...descriptor,
      kind: "refObject",
    } as RefObject;

    return { ...refObject, origin };
  };
}

export function mapRefObject(refObject: unknown): AppSpec.ExtImport {
  if (isNamedRefObject(refObject)) {
    return {
      kind: "named",
      name: refObject.import,
      source: mapRefObjectSource(refObject),
      alias: refObject.alias,
    };
  } else if (isDefaultRefObject(refObject)) {
    return {
      kind: "default",
      name: refObject.importDefault,
      source: mapRefObjectSource(refObject),
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

  if (!hasRefOrigin(refObject)) {
    throw new WaspSpecUserError(
      `Relative ref path ${JSON.stringify(refObject.from)} is missing spec file origin information. Use \`ref(...)\` in a \`*.wasp.ts\` file.`,
    );
  }

  const path = normalizeRefObjectPath({
    importPath: refObject.from,
    specFilePath: refObject.origin.specFilePath,
    originKind: refObject.origin.kind,
  });

  if (refObject.origin.kind === "package") {
    return {
      kind: "package",
      packageName: refObject.origin.packageName,
      subpath: path,
    };
  }

  return {
    kind: "project-src",
    path: `@src/${path}`,
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
  return posixPath.isAbsolute(path) || windowsPath.isAbsolute(path);
}

type RefObjectOrigin = {
  origin: RefOrigin;
};

type OriginAwareRefObject = RefObject & RefObjectOrigin;

function hasRefOrigin(value: unknown): value is RefObjectOrigin {
  if (!isObject(value) || !isObject(value.origin)) {
    return false;
  }

  const origin = value.origin;
  if (origin.kind === "project") {
    return typeof origin.specFilePath === "string";
  }

  return (
    origin.kind === "package" &&
    typeof origin.packageName === "string" &&
    origin.packageName.length > 0 &&
    typeof origin.specFilePath === "string"
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
