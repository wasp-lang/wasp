import * as ts from "typescript";

export function getImportStatements(
  sourceFile: ts.SourceFile,
): ts.ImportDeclaration[] {
  return sourceFile.statements.filter(ts.isImportDeclaration);
}

export function getLocalNameForValueImport(
  imports: ts.ImportDeclaration[],
  exportedName: string,
): string | undefined {
  for (const stmt of imports) {
    const specifier = getNamedValueImports(stmt)?.elements.find((specifier) =>
      isValueImportSpecifierFor(specifier, exportedName),
    );

    if (specifier !== undefined) {
      return specifier.name.text;
    }
  }

  return undefined;
}

export function getNamedImports(
  stmt: ts.ImportDeclaration,
): ts.NamedImports | undefined {
  const namedBindings = stmt.importClause?.namedBindings;
  return namedBindings && ts.isNamedImports(namedBindings)
    ? namedBindings
    : undefined;
}

export function getNamedValueImports(
  stmt: ts.ImportDeclaration,
): ts.NamedImports | undefined {
  const importClause = stmt.importClause;
  if (!importClause || importClause.isTypeOnly) {
    return undefined;
  }

  return getNamedImports(stmt);
}

export function isValueImportSpecifierFor(
  specifier: ts.ImportSpecifier,
  exportedName: string,
): boolean {
  return !specifier.isTypeOnly && isImportSpecifierFor(specifier, exportedName);
}

export function isImportSpecifierFor(
  specifier: ts.ImportSpecifier,
  exportedName: string,
): boolean {
  return getImportedName(specifier) === exportedName;
}

function getImportedName(specifier: ts.ImportSpecifier): string {
  return specifier.propertyName?.text ?? specifier.name.text;
}
