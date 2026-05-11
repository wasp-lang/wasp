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

const ACCEPTED_IMPORT_SHAPES =
  "Use default, named, aliased named, or namespace imports from @src/*.";

/**
 * Lowers top-level imports of the form `@src/*` into inline ExtImport consts
 * so the spec can reference user source modules without loading them at runtime.
 */
export function lowerSrcImports(sourceText: string): string {
  const plan = planImportLowering(sourceText);
  if (plan.status === "error") {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    throw new Error(formatImportDiagnostic(plan.error[0]!));
  }

  return renderPlan(sourceText, plan.value);
}

function renderPlan(text: string, plan: ImportLoweringPlan): string {
  let out = "";
  let cursor = 0;

  for (const replacement of plan.replacements) {
    out +=
      text.slice(cursor, replacement.start) +
      renderBindings(replacement.bindings);
    cursor = replacement.end;
  }

  return out + text.slice(cursor);
}

function renderBindings(bindings: LoweredImportBinding[]): string {
  return bindings.map(renderBinding).join("\n");
}

function renderBinding(binding: LoweredImportBinding): string {
  switch (binding.kind) {
    case "extImport":
      return `const ${binding.localName} = ${renderDescriptor(binding.extImport)} as const;`;
    case "namespace":
      return renderNamespaceProxy(binding);
  }
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `{ import: "anything", from: ... }`. This
 * avoids enumerating every place where `ops` is used.
 */
function renderNamespaceProxy(binding: NamespaceImportBinding): string {
  const from = JSON.stringify(binding.from);
  const aliasPrefix = JSON.stringify(binding.aliasPrefix);

  return `const ${binding.localName} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) } as const) }) as Record<string, { import: string; from: ${from}; alias: string }>;`;
}

function renderDescriptor(descriptor: ExtImport): string {
  const from = JSON.stringify(descriptor.from);
  const alias =
    "alias" in descriptor && descriptor.alias !== undefined
      ? JSON.stringify(descriptor.alias)
      : undefined;
  const aliasField = alias ? `, alias: ${alias}` : "";

  if (isNamedExtImport(descriptor)) {
    const importName = JSON.stringify(descriptor.import);

    return `{ import: ${importName}, from: ${from}${aliasField} }`;
  }

  const importDefault = JSON.stringify(descriptor.importDefault);

  return `{ importDefault: ${importDefault}, from: ${from}${aliasField} }`;
}

function formatImportDiagnostic(diagnostic: ImportDiagnostic): string {
  return `Unsupported @src import in main.wasp.ts at ${diagnostic.location.line}:${diagnostic.location.column}: ${formatUnsupportedImportType(diagnostic.unsupportedImportType)} Found ${JSON.stringify(diagnostic.specifier)}. ${ACCEPTED_IMPORT_SHAPES}`;
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
