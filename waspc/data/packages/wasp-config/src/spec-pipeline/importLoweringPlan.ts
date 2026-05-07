import * as ts from "typescript";
import type { ExtImportLiteral } from "../spec/extImport.js";

const SRC_IMPORT_PREFIX = "@src/";
const SRC_DESCRIPTOR_PREFIX = "@src/";

export type ImportLoweringResult =
  | { status: "ok"; plan: ImportLoweringPlan }
  | { status: "error"; diagnostics: ImportDiagnostic[] };

export type ImportLoweringPlan = {
  // Replacements are collected in source order so rendering can splice once.
  replacements: ImportReplacement[];
};

export type ImportReplacement = {
  start: number;
  end: number;
  declarations: DescriptorDeclaration[];
};

export type DescriptorDeclaration =
  | {
      kind: "descriptor";
      localName: string;
      descriptor: ExtImportLiteral;
    }
  | {
      kind: "namespace";
      localName: string;
      descriptorPath: ExtImportLiteral["from"];
      aliasPrefix: string;
    };

export type ImportDiagnostic = {
  reason: ImportDiagnosticReason;
  specifier: string;
  location: SourceLocation;
};

export type ImportDiagnosticReason =
  | "sideEffectImport"
  | "importEqualsImport"
  | "typeOnlyImport"
  | "mixedTypeAndValueImport"
  | "stringLiteralImport"
  | "emptyNamedImport"
  | "srcReExport";

export type SourceLocation = {
  line: number;
  column: number;
};

type StatementLowering =
  | { kind: "replacement"; replacement: ImportReplacement }
  | { kind: "diagnostic"; diagnostic: ImportDiagnostic };

// Import lowering turns authored @src imports into ExtImport descriptors so
// spec evaluation can reference user code without loading it at runtime.
export function planImportLowering(sourceText: string): ImportLoweringResult {
  const sourceFile = ts.createSourceFile(
    "input.ts",
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );

  const replacements: ImportReplacement[] = [];
  const diagnostics: ImportDiagnostic[] = [];

  for (const stmt of sourceFile.statements) {
    const lowering = planStatementLowering(sourceFile, stmt);
    if (!lowering) continue;

    if (lowering.kind === "diagnostic") {
      diagnostics.push(lowering.diagnostic);
    } else {
      replacements.push(lowering.replacement);
    }
  }

  if (diagnostics.length > 0) {
    return { status: "error", diagnostics };
  }

  return { status: "ok", plan: { replacements } };
}

function planStatementLowering(
  sourceFile: ts.SourceFile,
  stmt: ts.Statement,
): StatementLowering | undefined {
  if (ts.isImportEqualsDeclaration(stmt)) {
    const diagnostic = getImportEqualsDiagnostic(sourceFile, stmt);
    return diagnostic && { kind: "diagnostic", diagnostic };
  }

  if (ts.isExportDeclaration(stmt)) {
    const diagnostic = getSrcReExportDiagnostic(sourceFile, stmt);
    return diagnostic && { kind: "diagnostic", diagnostic };
  }

  if (!ts.isImportDeclaration(stmt)) {
    return undefined;
  }

  return planImportDeclarationLowering(sourceFile, stmt);
}

function planImportDeclarationLowering(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
): StatementLowering | undefined {
  const specifier = getStringModuleSpecifier(stmt.moduleSpecifier);
  if (!specifier || !isSrcImportSpecifier(specifier)) {
    return undefined;
  }

  const diagnostic = getSrcImportDiagnostic(sourceFile, stmt, specifier);
  if (diagnostic) {
    return { kind: "diagnostic", diagnostic };
  }

  const clause = stmt.importClause;
  if (!clause) return undefined;

  return {
    kind: "replacement",
    replacement: {
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      declarations: getDescriptorDeclarations(
        clause,
        toDescriptorPath(specifier),
      ),
    },
  };
}

function getStringModuleSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  return ts.isStringLiteral(moduleSpecifier) ? moduleSpecifier.text : undefined;
}

function isSrcImportSpecifier(specifier: string): boolean {
  return specifier.startsWith(SRC_IMPORT_PREFIX);
}

function getSrcImportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  specifier: string,
): ImportDiagnostic | undefined {
  const clause = stmt.importClause;
  if (!clause) {
    return makeDiagnostic(sourceFile, stmt, specifier, "sideEffectImport");
  }

  if (clause.isTypeOnly) {
    return makeDiagnostic(sourceFile, stmt, specifier, "typeOnlyImport");
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamedImports(bindings)) {
    if (!clause.name && bindings.elements.length === 0) {
      return makeDiagnostic(sourceFile, stmt, specifier, "emptyNamedImport");
    }

    if (bindings.elements.some((element) => element.isTypeOnly)) {
      return makeDiagnostic(
        sourceFile,
        stmt,
        specifier,
        "mixedTypeAndValueImport",
      );
    }

    if (bindings.elements.some(hasStringLiteralImportedName)) {
      return makeDiagnostic(sourceFile, stmt, specifier, "stringLiteralImport");
    }
  }

  return undefined;
}

function hasStringLiteralImportedName(spec: ts.ImportSpecifier): boolean {
  return (
    spec.propertyName !== undefined && ts.isStringLiteral(spec.propertyName)
  );
}

function getImportEqualsDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportEqualsDeclaration,
): ImportDiagnostic | undefined {
  const specifier = getImportEqualsSpecifier(stmt);
  if (!specifier || !isSrcImportSpecifier(specifier)) {
    return undefined;
  }

  return makeDiagnostic(sourceFile, stmt, specifier, "importEqualsImport");
}

function getImportEqualsSpecifier(
  stmt: ts.ImportEqualsDeclaration,
): string | undefined {
  const moduleReference = stmt.moduleReference;
  if (!ts.isExternalModuleReference(moduleReference)) {
    return undefined;
  }

  return getStringModuleSpecifier(moduleReference.expression);
}

function getSrcReExportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): ImportDiagnostic | undefined {
  if (!stmt.moduleSpecifier || !ts.isStringLiteral(stmt.moduleSpecifier)) {
    return undefined;
  }

  const specifier = stmt.moduleSpecifier.text;
  if (!isSrcImportSpecifier(specifier)) {
    return undefined;
  }

  return makeDiagnostic(sourceFile, stmt, specifier, "srcReExport");
}

function makeDiagnostic(
  sourceFile: ts.SourceFile,
  node: ts.Node,
  specifier: string,
  reason: ImportDiagnosticReason,
): ImportDiagnostic {
  const { line, character } = sourceFile.getLineAndCharacterOfPosition(
    node.getStart(sourceFile),
  );
  return {
    reason,
    specifier,
    location: { line: line + 1, column: character + 1 },
  };
}

function getDescriptorDeclarations(
  clause: ts.ImportClause,
  descriptorPath: ExtImportLiteral["from"],
): DescriptorDeclaration[] {
  const declarations: DescriptorDeclaration[] = [];

  if (clause.name) {
    const name = clause.name.text;
    declarations.push({
      kind: "descriptor",
      localName: name,
      descriptor: { importDefault: name, from: descriptorPath },
    });
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamespaceImport(bindings)) {
    const name = bindings.name.text;
    declarations.push({
      kind: "namespace",
      localName: name,
      descriptorPath,
      aliasPrefix: getNamespaceAliasPrefix(name),
    });
  } else if (bindings) {
    for (const spec of bindings.elements) {
      const localName = spec.name.text;
      const importedName = getImportedName(spec);
      declarations.push({
        kind: "descriptor",
        localName,
        descriptor: {
          import: importedName,
          from: descriptorPath,
          ...(importedName === localName ? {} : { alias: localName }),
        },
      });
    }
  }

  return declarations;
}

function getNamespaceAliasPrefix(namespaceName: string): string {
  // Namespace imports synthesize descriptors on property access. Prefix aliases
  // with the namespace name so `ops.archive` and `legacyOps.archive` do not
  // collide after lowering.
  return `${namespaceName}_`;
}

function getImportedName(spec: ts.ImportSpecifier): string {
  // In `import { exported as local }`, TS stores `exported` in `propertyName`
  // and `local` in `name`. For `import { local }`, `propertyName` is undefined.
  return spec.propertyName?.text ?? spec.name.text;
}

function toDescriptorPath(specifier: string): ExtImportLiteral["from"] {
  return (SRC_DESCRIPTOR_PREFIX +
    specifier.slice(SRC_IMPORT_PREFIX.length)) as ExtImportLiteral["from"];
}
