import type * as AppSpec from "../appSpec.js";
import type { Result } from "../result.js";

export type ExtImport = NamedExtImport | DefaultExtImport;

export interface NamedExtImport {
  import: string;
  alias?: string;
  from: AppSpec.ExtImport["path"];
}

export interface DefaultExtImport {
  importDefault: string;
  from: AppSpec.ExtImport["path"];
}

export function mapExtImport(extImport: unknown): AppSpec.ExtImport {
  const result = tryMapExtImport(extImport);
  if (result.status === "ok") return result.value;

  throw new Error(result.error);
}

export function tryMapExtImport(
  extImport: unknown,
): Result<AppSpec.ExtImport, string> {
  if (isNamedExtImport(extImport)) {
    return {
      status: "ok",
      value: {
        kind: "named",
        name: extImport.import,
        path: extImport.from,
        alias: extImport.alias,
      },
    };
  } else if (isDefaultExtImport(extImport)) {
    return {
      status: "ok",
      value: {
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
      },
    };
  } else {
    return {
      status: "error",
      error: invalidExtImportError,
    };
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

const invalidExtImportError =
  "Invalid ExtImport value: got a runtime value that is not a valid ExtImport. " +
  "Import values from @src/* so Wasp can rewrite them automatically, " +
  "or provide an ExtImport object directly: " +
  "{ import, from, alias } or { importDefault, from } (alias is optional for named imports).";

function hasValidAlias(value: Record<string, unknown>): boolean {
  return value.alias === undefined || typeof value.alias === "string";
}

function isObject(value: unknown): value is Record<string, unknown> {
  return Object.prototype.toString.call(value) === "[object Object]";
}
