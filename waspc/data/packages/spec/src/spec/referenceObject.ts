import type * as AppSpec from "../appSpec.js";
import { SpecUserError } from "./specUserError.js";

export interface ReferenceObject {
  import: string;
  alias?: string;
  from: AppSpec.ExtImport["path"];
}

export const DEFAULT_IMPORT_NAME = "default";

export function mapReferenceObject(ref: unknown): AppSpec.ExtImport {
  if (!isReferenceObject(ref)) {
    throw new SpecUserError(
      "Got a reference in the Wasp file that we couldn't process: " +
        JSON.stringify(ref) +
        '\nYou either used a value you imported from outside of "@src/" or didn\'t write the ReferenceObject correctly.',
    );
  }

  if (ref.import === DEFAULT_IMPORT_NAME) {
    if (ref.alias === undefined) {
      throw new SpecUserError(
        `Default imports must include an \`alias\` for the local binding name. Got: ${JSON.stringify(ref)}`,
      );
    }
    return {
      kind: "default",
      name: ref.alias,
      path: ref.from,
    };
  }

  return {
    kind: "named",
    name: ref.import,
    path: ref.from,
    alias: ref.alias,
  };
}

export function isReferenceObject(value: unknown): value is ReferenceObject {
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
