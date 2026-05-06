import type { ExtImportDescriptor } from "../appSpec/extImportDescriptor.js";
import type {
  DescriptorDeclaration,
  ImportDiagnostic,
  ImportDiagnosticReason,
  ImportLoweringPlan,
} from "./importLoweringPlan.js";
import { planImportLowering } from "./importLoweringPlan.js";

const ACCEPTED_IMPORT_SHAPES =
  "Use default, named, aliased named, or namespace imports from ./src/*.";
const SUPPORTED_IMPORTS =
  "Use package imports for dependencies and ./src/* imports for external Wasp code references.";

/**
 * Rewrites top-level imports of the form `./src/*` into inline ExtImport
 * descriptor consts so the file no longer depends on user source modules at
 * runtime. The rewriter is purely textual: it parses a single SourceFile,
 * collects edit ranges for matching ImportDeclarations, and splices them.
 */
export function rewrite(sourceText: string): string {
  const result = planImportLowering(sourceText);
  if (result.status === "error") {
    throw new Error(formatImportDiagnostic(result.diagnostics[0]!));
  }

  return renderPlan(sourceText, result.plan);
}

function renderPlan(text: string, plan: ImportLoweringPlan): string {
  const replacements = [...plan.replacements].sort((a, b) => a.start - b.start);
  let out = "";
  let cursor = 0;

  for (const replacement of replacements) {
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
      return `const ${declaration.localName} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${JSON.stringify(declaration.descriptorPath)}, alias: ${JSON.stringify(declaration.aliasPrefix)} + String(k) } as const) }) as Record<string, { import: string; from: ${JSON.stringify(declaration.descriptorPath)}; alias: string }>;`;
  }
}

function renderDescriptor(descriptor: ExtImportDescriptor): string {
  if ("import" in descriptor) {
    return `{ import: ${JSON.stringify(descriptor.import)}, from: ${JSON.stringify(descriptor.from)}${renderAlias(descriptor)} }`;
  }

  return `{ importDefault: ${JSON.stringify(descriptor.importDefault)}, from: ${JSON.stringify(descriptor.from)}${renderAlias(descriptor)} }`;
}

function renderAlias(descriptor: ExtImportDescriptor): string {
  return "alias" in descriptor && descriptor.alias !== undefined
    ? `, alias: ${JSON.stringify(descriptor.alias)}`
    : "";
}

function formatImportDiagnostic(diagnostic: ImportDiagnostic): string {
  return `Unsupported ./src import in main.wasp.ts at ${diagnostic.location.line}:${diagnostic.location.column}: ${formatImportDiagnosticReason(diagnostic.reason)} Found ${JSON.stringify(diagnostic.specifier)}. ${ACCEPTED_IMPORT_SHAPES}`;
}

function formatImportDiagnosticReason(reason: ImportDiagnosticReason): string {
  switch (reason) {
    case "relativeImportOutsideSrc":
      return `Relative imports outside ./src/* are not supported. ${SUPPORTED_IMPORTS}`;
    case "relativeReExportOutsideSrc":
      return `Relative re-exports are not supported. ${SUPPORTED_IMPORTS}`;
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
