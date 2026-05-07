import type * as AppSpec from "../appSpec.js";

export type ExtImport = NamedExtImport | DefaultExtImport;

export interface NamedExtImport {
  import: string;
  alias?: string;
  from: AppSpec.ExtImport["path"];
}

export interface DefaultExtImport {
  importDefault: string;
  from: AppSpec.ExtImport["path"];
  alias?: string;
}

export function mapExtImport(extImport: ExtImport): AppSpec.ExtImport {
  if (isNamedExtImport(extImport)) {
    return {
      kind: "named",
      name: extImport.import,
      path: extImport.from,
      ...mapAlias(extImport),
    };
  } else if (isDefaultExtImport(extImport)) {
    return {
      kind: "default",
      name: extImport.importDefault,
      path: extImport.from,
      ...mapAlias(extImport),
    };
  } else {
    throw new Error(
      "Invalid ExtImport: neither `import` nor `importDefault` is defined",
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

function mapAlias(
  extImport: ExtImport,
): Partial<Pick<AppSpec.ExtImport, "alias">> {
  return extImport.alias !== undefined ? { alias: extImport.alias } : {};
}
