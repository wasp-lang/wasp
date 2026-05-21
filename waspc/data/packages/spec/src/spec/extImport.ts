import type * as AppSpec from "../appSpec.js";
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

export function mapExtImport(extImport: unknown): AppSpec.ExtImport {
  if (isNamedExtImport(extImport)) {
    return {
      kind: "named",
      name: extImport.import,
      path: extImport.from,
      alias: extImport.alias,
    };
  } else if (isDefaultExtImport(extImport)) {
    return {
      kind: "default",
      name: extImport.importDefault,
      path: extImport.from,
    };
  } else {
    throw new SpecUserError(
      "Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(extImport) +
        '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the ExtImport object correctly.',
    );
  }
}

export function isNamedExtImport(value: unknown): value is NamedExtImport {
  return (
    isObject(value) &&
    typeof value.import === "string" &&
    typeof value.from === "string" &&
    hasValidAlias(value)
  );
}

function isDefaultExtImport(value: unknown): value is DefaultExtImport {
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
