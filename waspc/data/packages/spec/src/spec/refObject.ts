import type * as AppSpec from "../appSpec.js";
import { SpecUserError } from "./specUserError.js";

/**
 * A reference object pointing to code in your app's `src` directory.
 *
 * Use this when you can't use a reference import.
 * The `from` path must start with `@src/`. The `import` field selects which
 * export to use: `"default"` for a default import (a {@link DefaultRefObject},
 * with the local name in `alias`), or any other string for a named export
 * (a {@link NamedRefObject}, optionally renamed via `alias`).
 */
export type RefObject = NamedRefObject | DefaultRefObject;

/**
 * Named import reference, equivalent to
 * `import { <import> as <alias> } from "<from>" with { type: "ref" }`.
 *
 * `import` is the exported name (anything except `"default"`); `alias` is
 * optional and renames the value locally.
 */
export interface NamedRefObject {
  /** Exported name to import. Must not be `"default"`. */
  import: string;
  /**
   * Optional local alias.
   *
   * When Wasp derives a declaration name from this reference, the alias takes
   * precedence over the `import` field.
   */
  alias?: string;
  /** Module path. Must start with `@src/`. */
  from: AppSpec.ExtImport["path"];
}

/**
 * Default import reference, equivalent to
 * `import <alias> from "<from>" with { type: "ref" }`.
 */
export interface DefaultRefObject {
  /** Marks this object as a default import. */
  import: "default";
  /** Local name for the default import. */
  alias: string;
  /** Module path. Must start with `@src/`. */
  from: AppSpec.ExtImport["path"];
}

export function mapRefObject(refObject: unknown): AppSpec.ExtImport {
  if (isDefaultRefObject(refObject)) {
    return {
      kind: "default",
      name: refObject.alias,
      path: refObject.from,
    };
  } else if (isNamedRefObject(refObject)) {
    return {
      kind: "named",
      name: refObject.import,
      path: refObject.from,
      alias: refObject.alias,
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
    value.import !== "default" &&
    typeof value.from === "string" &&
    hasValidAlias(value)
  );
}

export function isDefaultRefObject(value: unknown): value is DefaultRefObject {
  return (
    isObject(value) &&
    value.import === "default" &&
    typeof value.alias === "string" &&
    typeof value.from === "string"
  );
}

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
