import type { ExtImportDescriptor } from "../appSpec/extImportDescriptor.js";
import { isNamedExtImportDescriptor } from "../appSpec/extImportDescriptor.js";
import type {
  DescriptorDeclaration,
  ImportDiagnostic,
  ImportDiagnosticReason,
  ImportLoweringPlan,
} from "./importLoweringPlan.js";
import { planImportLowering } from "./importLoweringPlan.js";

const ACCEPTED_IMPORT_SHAPES =
  "Use default, named, aliased named, or namespace imports from @src/*.";

/**
 * Lowers top-level imports of the form `@src/*` into inline ExtImport
 * descriptor consts so the spec can reference user source modules without
 * loading them at runtime.
 */
export function lowerSrcImports(sourceText: string): string {
  const lowering = planImportLowering(sourceText);
  if (lowering.status === "error") {
    throw new Error(formatImportDiagnostic(lowering.diagnostics[0]!));
  }

  return renderPlan(sourceText, lowering.plan);
}

function renderPlan(text: string, plan: ImportLoweringPlan): string {
  let out = "";
  let cursor = 0;

  for (const replacement of plan.replacements) {
    out +=
      text.slice(cursor, replacement.start) +
      renderDeclarations(replacement.declarations);
    cursor = replacement.end;
  }

  return out + text.slice(cursor);
}

function renderDeclarations(declarations: DescriptorDeclaration[]): string {
  return declarations.map(renderDeclaration).join("\n");
}

function renderDeclaration(declaration: DescriptorDeclaration): string {
  switch (declaration.kind) {
    case "descriptor":
      return `const ${declaration.localName} = ${renderDescriptor(declaration.descriptor)} as const;`;
    case "namespace":
      return renderNamespaceProxy(declaration);
  }
}

function renderNamespaceProxy(
  declaration: Extract<DescriptorDeclaration, { kind: "namespace" }>,
): string {
  const from = JSON.stringify(declaration.descriptorPath);
  const aliasPrefix = JSON.stringify(declaration.aliasPrefix);

  return `const ${declaration.localName} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) } as const) }) as Record<string, { import: string; from: ${from}; alias: string }>;`;
}

function renderDescriptor(descriptor: ExtImportDescriptor): string {
  const from = JSON.stringify(descriptor.from);
  const alias =
    "alias" in descriptor && descriptor.alias !== undefined
      ? JSON.stringify(descriptor.alias)
      : undefined;
  const aliasField = alias ? `, alias: ${alias}` : "";

  if (isNamedExtImportDescriptor(descriptor)) {
    const importName = JSON.stringify(descriptor.import);

    return `{ import: ${importName}, from: ${from}${aliasField} }`;
  }

  const importDefault = JSON.stringify(descriptor.importDefault);

  return `{ importDefault: ${importDefault}, from: ${from}${aliasField} }`;
}

function formatImportDiagnostic(diagnostic: ImportDiagnostic): string {
  return `Unsupported @src import in main.wasp.ts at ${diagnostic.location.line}:${diagnostic.location.column}: ${formatImportDiagnosticReason(diagnostic.reason)} Found ${JSON.stringify(diagnostic.specifier)}. ${ACCEPTED_IMPORT_SHAPES}`;
}

function formatImportDiagnosticReason(reason: ImportDiagnosticReason): string {
  switch (reason) {
    case "sideEffectImport":
      return "Side-effect imports are not supported.";
    case "typeOnlyImport":
      return "Type-only imports are not supported.";
    case "mixedTypeAndValueImport":
      return "Mixed type/value imports are not supported.";
    case "emptyNamedImport":
      return "Empty named imports are not supported.";
    case "srcReExport":
      return "Re-exports are not supported.";
  }
}
