import * as ts from "typescript";
import { getImportStatements } from "./importStatements.js";

export const SPEC_PACKAGE_NAME = "@wasp.sh/spec";
export const SPEC_PACKAGE_INTERNAL_NAME = "@wasp.sh/spec/internal";
export const REF_EXPORT_NAME = "ref";
export const REF_IMPORT_FACTORY_EXPORT_NAME = "_waspMakeRef";

export function getSpecPackageImports(
  sourceFile: ts.SourceFile,
): ts.ImportDeclaration[] {
  return getImportStatements(sourceFile).filter(isSpecPackageImport);
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

type ImportSpecifierSource = ts.ImportSpecifier | string;

export function getSpecPackageImportStatementSource(
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
