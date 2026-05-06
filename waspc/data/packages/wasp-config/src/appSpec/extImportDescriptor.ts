import type * as AppSpec from "../appSpec.js";

export type ExtImportDescriptor =
  | {
      import: string;
      from: AppSpec.ExtImport["path"];
      alias?: string;
    }
  | {
      importDefault: string;
      from: AppSpec.ExtImport["path"];
      alias?: string;
    };

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type AnyFunction = (...args: any[]) => any;

type NonDescriptorObject = object & {
  import?: never;
  importDefault?: never;
  from?: never;
  call?: never;
  apply?: never;
  bind?: never;
};

export type FunctionExtImport = ExtImportDescriptor | AnyFunction;
export type ObjectExtImport = ExtImportDescriptor | NonDescriptorObject;
export type AuthoredExtImport = FunctionExtImport | ObjectExtImport;

export type ExtImportDescriptorResult =
  | { status: "ok"; value: AppSpec.ExtImport }
  | { status: "error"; reason: ExtImportDescriptorErrorReason };

export type ExtImportDescriptorErrorReason =
  | "functionValue"
  | "descriptorLikeObject"
  | "objectValue";

type NamedExtImportDescriptor = Extract<
  ExtImportDescriptor,
  { import: string }
>;
type DefaultExtImportDescriptor = Extract<
  ExtImportDescriptor,
  { importDefault: string }
>;

export function mapAuthoredExtImport(
  extImport: AuthoredExtImport,
): ExtImportDescriptorResult {
  if (isNamedExtImportDescriptor(extImport)) {
    return {
      status: "ok",
      value: {
        kind: "named",
        name: extImport.import,
        path: extImport.from,
        ...mapAlias(extImport),
      },
    };
  }

  if (isDefaultExtImportDescriptor(extImport)) {
    return {
      status: "ok",
      value: {
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
        ...mapAlias(extImport),
      },
    };
  }

  return { status: "error", reason: getInvalidDescriptorReason(extImport) };
}

export function formatExtImportDescriptorError(
  reason: ExtImportDescriptorErrorReason,
): string {
  switch (reason) {
    case "functionValue":
      return (
        "Invalid ExtImport value: got a function at runtime. " +
        "Import-form values must come from a supported top-level ./src/* import so Wasp can rewrite them before execution. " +
        'Alternatively, use descriptor form: { import: "name", from: "@src/file" }.'
      );
    case "descriptorLikeObject":
      return (
        'Invalid ExtImport descriptor: expected either { import: "name", from: "@src/file" } ' +
        'or { importDefault: "name", from: "@src/file" }.'
      );
    case "objectValue":
      return (
        "Invalid ExtImport value: got an object at runtime. " +
        "Import-form values must come from a supported top-level ./src/* import so Wasp can rewrite them before execution. " +
        'Alternatively, use descriptor form: { import: "name", from: "@src/file" }.'
      );
  }
}

function isNamedExtImportDescriptor(
  extImport: AuthoredExtImport,
): extImport is NamedExtImportDescriptor {
  return (
    typeof extImport === "object" &&
    extImport !== null &&
    "import" in extImport &&
    "from" in extImport &&
    typeof extImport.import === "string" &&
    typeof extImport.from === "string" &&
    hasValidAlias(extImport)
  );
}

function isDefaultExtImportDescriptor(
  extImport: AuthoredExtImport,
): extImport is DefaultExtImportDescriptor {
  return (
    typeof extImport === "object" &&
    extImport !== null &&
    "importDefault" in extImport &&
    "from" in extImport &&
    typeof extImport.importDefault === "string" &&
    typeof extImport.from === "string" &&
    hasValidAlias(extImport)
  );
}

function hasValidAlias(extImport: object): boolean {
  return !("alias" in extImport) || typeof extImport.alias === "string";
}

function mapAlias(
  extImport: ExtImportDescriptor,
): Partial<Pick<AppSpec.ExtImport, "alias">> {
  return "alias" in extImport && extImport.alias !== undefined
    ? { alias: extImport.alias }
    : {};
}

function getInvalidDescriptorReason(
  extImport: AuthoredExtImport,
): ExtImportDescriptorErrorReason {
  if (typeof extImport === "function") {
    return "functionValue";
  }

  if (isDescriptorLikeObject(extImport)) {
    return "descriptorLikeObject";
  }

  return "objectValue";
}

function isDescriptorLikeObject(extImport: AuthoredExtImport): boolean {
  return (
    typeof extImport === "object" &&
    extImport !== null &&
    ("import" in extImport ||
      "importDefault" in extImport ||
      "from" in extImport)
  );
}
