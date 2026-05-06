import * as ts from "typescript";
import type { ExtImportDescriptor } from "../appSpec/extImportDescriptor.js";

const SRC_IMPORT_PREFIX = "./src/";
const SRC_DESCRIPTOR_PREFIX = "@src/";

export type ImportLoweringResult =
  | { status: "ok"; plan: ImportLoweringPlan }
  | { status: "error"; diagnostics: ImportDiagnostic[] };

export type ImportLoweringPlan = {
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
      descriptor: ExtImportDescriptor;
    }
  | {
      kind: "namespace";
      localName: string;
      descriptorPath: ExtImportDescriptor["from"];
      aliasPrefix: string;
    };

export type ImportDiagnostic = {
  reason: ImportDiagnosticReason;
  specifier: string;
  location: SourceLocation;
};

export type ImportDiagnosticReason =
  | "sideEffectImport"
  | "typeOnlyImport"
  | "mixedTypeAndValueImport"
  | "emptyNamedImport"
  | "srcReExport"
  | "relativeImportOutsideSrc"
  | "relativeReExportOutsideSrc";

export type SourceLocation = {
  line: number;
  column: number;
};

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
    if (ts.isExportDeclaration(stmt)) {
      const diagnostic = getExportDiagnostic(sourceFile, stmt);
      if (diagnostic) diagnostics.push(diagnostic);
      continue;
    }

    if (!ts.isImportDeclaration(stmt)) continue;
    const specifier = getStringModuleSpecifier(stmt.moduleSpecifier);
    if (!specifier) continue;

    if (!isSrcImportSpecifier(specifier)) {
      const diagnostic = getNonSrcImportDiagnostic(sourceFile, stmt, specifier);
      if (diagnostic) diagnostics.push(diagnostic);
      continue;
    }

    const diagnostic = getSrcImportDiagnostic(sourceFile, stmt, specifier);
    if (diagnostic) {
      diagnostics.push(diagnostic);
      continue;
    }

    const clause = stmt.importClause;
    if (!clause) continue;

    replacements.push({
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      declarations: getDescriptorDeclarations(
        clause,
        toDescriptorPath(specifier),
      ),
    });
  }

  if (diagnostics.length > 0) {
    return { status: "error", diagnostics };
  }

  return { status: "ok", plan: { replacements } };
}

function getStringModuleSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  return ts.isStringLiteral(moduleSpecifier) ? moduleSpecifier.text : undefined;
}

function isSrcImportSpecifier(specifier: string): boolean {
  return specifier.startsWith(SRC_IMPORT_PREFIX);
}

function isRelativeImportSpecifier(specifier: string): boolean {
  return specifier.startsWith("./") || specifier.startsWith("../");
}

function getNonSrcImportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  specifier: string,
): ImportDiagnostic | undefined {
  if (!isRelativeImportSpecifier(specifier)) return undefined;

  return makeDiagnostic(
    sourceFile,
    stmt,
    specifier,
    "relativeImportOutsideSrc",
  );
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
  }

  return undefined;
}

function getExportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): ImportDiagnostic | undefined {
  if (!stmt.moduleSpecifier || !ts.isStringLiteral(stmt.moduleSpecifier)) {
    return undefined;
  }

  const specifier = stmt.moduleSpecifier.text;
  if (!isSrcImportSpecifier(specifier)) {
    return isRelativeImportSpecifier(specifier)
      ? makeDiagnostic(
          sourceFile,
          stmt,
          specifier,
          "relativeReExportOutsideSrc",
        )
      : undefined;
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
  descriptorPath: ExtImportDescriptor["from"],
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
      aliasPrefix: `${name}_`,
    });
  } else if (bindings) {
    for (const spec of bindings.elements) {
      const localName = spec.name.text;
      const importedName = spec.propertyName?.text ?? localName;
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

function toDescriptorPath(specifier: string): ExtImportDescriptor["from"] {
  return (SRC_DESCRIPTOR_PREFIX +
    specifier.slice(SRC_IMPORT_PREFIX.length)) as ExtImportDescriptor["from"];
}
