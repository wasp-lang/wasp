import type { ExtImport } from "../spec/extImport.js";
import { isNamedExtImport } from "../spec/extImport.js";
import type { ImportLoweringPlan } from "./planImportLowering/index.js";
import { planImportLowering } from "./planImportLowering/index.js";
import type {
  LoweredImportBinding,
  NamespaceImportBinding,
} from "./planImportLowering/loweredImportBindings.js";
import type {
  ImportDiagnostic,
  UnsupportedImportType,
} from "./planImportLowering/supportedImportTypes.js";

/**
 * Given source code, finds supported @src import statements and replaces them
 * with inline ExtImport consts. We call this lowering imports.
 */
export function lowerSrcImports({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): string {
  const plan = planImportLowering({ sourceText, sourcePath });
  if (plan.status === "error") {
    throw new Error(formatImportDiagnostics(plan.error));
  }

  return applyImportLoweringPlan(sourceText, plan.value);
}

function applyImportLoweringPlan(
  sourceText: string,
  plan: ImportLoweringPlan,
): string {
  let sourceCursor = 0;
  let modifiedSource = "";

  for (const replacement of plan.replacements) {
    modifiedSource += sourceText.slice(sourceCursor, replacement.start);
    modifiedSource += getImportReplacementSource(replacement.bindings);
    sourceCursor = replacement.end;
  }

  modifiedSource += sourceText.slice(sourceCursor);

  return modifiedSource;
}

function getImportReplacementSource(bindings: LoweredImportBinding[]): string {
  return bindings.map(getLoweredImportBindingSource).join("\n");
}

function getLoweredImportBindingSource(binding: LoweredImportBinding): string {
  switch (binding.kind) {
    case "extImport":
      return `const ${binding.localName} = ${getExtImportObjectLiteralSource(binding.extImport)} as const;`;
    case "namespace":
      return getNamespaceImportProxySource(binding);
  }
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `{ import: "anything", from: ... }`. This
 * avoids enumerating every place where `ops` is used.
 */
function getNamespaceImportProxySource(
  binding: NamespaceImportBinding,
): string {
  const from = JSON.stringify(binding.from);
  const aliasPrefix = JSON.stringify(binding.aliasPrefix);

  return `const ${binding.localName} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) } as const) }) as Record<string, { import: string; from: ${from}; alias: string }>;`;
}

function getExtImportObjectLiteralSource(extImport: ExtImport): string {
  if (isNamedExtImport(extImport)) {
    const fields: Field[] = [
      ["import", extImport.import],
      ["from", extImport.from],
      ["alias", extImport.alias],
    ];

    return `{ ${getObjectFieldsSource(fields)} }`;
  } else {
    const fields: Field[] = [
      ["importDefault", extImport.importDefault],
      ["from", extImport.from],
    ];

    return `{ ${getObjectFieldsSource(fields)} }`;
  }
}

/**
 * [["key", "value"], ["key2", "value2"]] => "key: "value", key2: "value2""
 */
function getObjectFieldsSource(fields: Field[]): string {
  return fields
    .filter(([_, value]) => value !== undefined)
    .map(([key, value]) => `${key}: ${JSON.stringify(value)}`)
    .join(", ");
}

type Field = [string, string | undefined];

function formatImportDiagnostics(diagnostics: ImportDiagnostic[]): string {
  return [
    ...diagnostics.map(formatImportDiagnosticLine),
    "",
    "Supported @src imports are default, named, aliased named, or namespace imports from @src/*.",
  ].join("\n");
}

function formatImportDiagnosticLine(diagnostic: ImportDiagnostic): string {
  return `${diagnostic.filePath}(${diagnostic.location.line},${diagnostic.location.column}): error: Unsupported @src import ${JSON.stringify(diagnostic.specifier)}. ${formatUnsupportedImportType(diagnostic.unsupportedImportType)}`;
}

function formatUnsupportedImportType(type: UnsupportedImportType): string {
  switch (type) {
    case "sideEffect":
      return "Side-effect imports are not supported.";
    case "importEquals":
      return "Import equals declarations are not supported.";
    case "typeOnly":
      return "Type-only imports are not supported.";
    case "mixedTypeAndValue":
      return "Mixed type/value imports are not supported.";
    case "stringLiteral":
      return "String-literal named imports are not supported.";
    case "emptyNamed":
      return "Empty named imports are not supported.";
    case "reExport":
      return "Re-exports are not supported.";
  }
}
