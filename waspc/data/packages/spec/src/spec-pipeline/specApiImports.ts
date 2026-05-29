import * as ts from "typescript";

export const PACKAGE_SPEC_MODULE_NAME = "@wasp.sh/spec";
export const REF_IMPORT_NAME = "refImport";
export const MAKE_REF_IMPORT_NAME = "makeRefImport";

export type ImportSpecifierSource = ts.ImportSpecifier | string;

export function getSpecApiImports(
  sourceFile: ts.SourceFile,
): ts.ImportDeclaration[] {
  return sourceFile.statements
    .filter(ts.isImportDeclaration)
    .filter(isSpecImportDeclaration);
}

export function findValueImportLocalName(
  imports: ts.ImportDeclaration[],
  exportedName: string,
): string | undefined {
  for (const stmt of imports) {
    const importClause = stmt.importClause;
    if (!importClause || importClause.isTypeOnly) {
      continue;
    }

    const namedBindings = importClause.namedBindings;
    if (!namedBindings || !ts.isNamedImports(namedBindings)) {
      continue;
    }

    const specifier = namedBindings.elements.find((specifier) =>
      isValueImportOf(specifier, exportedName),
    );
    if (specifier) {
      return specifier.name.text;
    }
  }

  return undefined;
}

export function isValueImportOf(
  specifier: ts.ImportSpecifier,
  exportedName: string,
): boolean {
  return !specifier.isTypeOnly && getImportedName(specifier) === exportedName;
}

export function formatSpecApiImport(
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

function isSpecImportDeclaration(stmt: ts.ImportDeclaration): boolean {
  return (
    ts.isStringLiteral(stmt.moduleSpecifier) &&
    isSpecModuleSpecifier(stmt.moduleSpecifier.text)
  );
}

function isSpecModuleSpecifier(moduleSpecifier: string): boolean {
  return (
    moduleSpecifier === PACKAGE_SPEC_MODULE_NAME ||
    /\/src\/spec\/publicApi\/index\.[jt]s$/.test(moduleSpecifier)
  );
}
