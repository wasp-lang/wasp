import type * as AppSpec from "../appSpec.js";
import { SpecUserError } from "./specUserError.js";

/**
 * A reference to code in your app's `src` directory.
 *
 * Use this when you can't use a reference import.
 * The import path must start with `@src/` and be either a single named import
 * ({@link NamedRefObject}) or a default import ({@link DefaultRefObject}).
 */
export type RefObject = NamedRefObject | DefaultRefObject;

/**
 * Named import reference, equivalent to
 * `import { SomeValue } from "./src/someModule" with { type: "ref" }`.
 */
export interface NamedRefObject {
  /** Exported name to import. */
  import: string;
  /**
   * Optional local alias.
   *
   * When Wasp derives a specification name from this import, the alias takes
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
export interface DefaultRefObject {
  /** Local name for the default import. */
  importDefault: string;
  /** Module path. Must start with `@src/`. */
  from: AppSpec.ExtImport["path"];
}

export function mapRefObject(refObject: unknown): AppSpec.ExtImport {
  if (isNamedRefObject(refObject)) {
    return {
      kind: "named",
      name: refObject.import,
      path: refObject.from,
      alias: refObject.alias,
    };
  } else if (isDefaultRefObject(refObject)) {
    return {
      kind: "default",
      name: refObject.importDefault,
      path: refObject.from,
    };
  } else {
    throw new SpecUserError(
      "Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(refObject) +
        '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the RefObject correctly.',
    );
  }
}

export function isNamedRefObject(value: unknown): value is NamedRefObject {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value)
  );
}

function isDefaultRefObject(value: unknown): value is DefaultRefObject {
  return (
    isObject(value) &&
    typeof value.importDefault === "string" &&
    typeof value.from === "string"
  );
}

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
