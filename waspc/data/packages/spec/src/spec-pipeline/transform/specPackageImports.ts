import * as ts from "typescript";

export const SPEC_PACKAGE_NAME = "@wasp.sh/spec";
export const REF_EXPORT_NAME = "ref";
export const REF_IMPORT_FACTORY_EXPORT_NAME = "_waspMakeRef";

export function getSpecPackageImports(
  sourceFile: ts.SourceFile,
): ts.ImportDeclaration[] {
  return sourceFile.statements
    .filter(ts.isImportDeclaration)
    .filter(isSpecPackageImport);
}

function isSpecPackageImport(stmt: ts.ImportDeclaration): boolean {
  return (
    ts.isStringLiteral(stmt.moduleSpecifier) &&
    isSpecPackageSpecifier(stmt.moduleSpecifier.text)
  );
}

function isSpecPackageSpecifier(moduleSpecifier: string): boolean {
  return moduleSpecifier === SPEC_PACKAGE_NAME;
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

type ImportSpecifierSource = ts.ImportSpecifier | string;

export function formatSpecPackageImport(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  namedSpecifiers: ImportSpecifierSource[],
): string {
  const importClause = stmt.importClause;
  if (!importClause) {
    return "";
  }

  const defaultImport = importClause.name?.text;
  const namedImports = namedSpecifiers.map((specifier) =>
    typeof specifier === "string" ? specifier : specifier.getText(sourceFile),
  );

  if (!defaultImport && namedImports.length === 0) {
    return "";
  }

  const importParts = [
    defaultImport,
    namedImports.length > 0 ? `{ ${namedImports.join(", ")} }` : undefined,
  ].filter((part): part is string => part !== undefined);

  return `import ${importParts.join(", ")} from ${stmt.moduleSpecifier.getText(sourceFile)};`;
}

function getImportedName(specifier: ts.ImportSpecifier): string {
  return specifier.propertyName?.text ?? specifier.name.text;
}
