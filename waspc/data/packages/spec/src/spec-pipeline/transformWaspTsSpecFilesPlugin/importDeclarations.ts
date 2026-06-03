import type { ESTree as t } from "rolldown/utils";

export type NamedImportSpecifier = Extract<
  t.ImportDeclarationSpecifier,
  { type: "ImportSpecifier" }
>;

export function isImportDeclaration(
  stmt: t.Statement,
): stmt is t.ImportDeclaration {
  return stmt.type === "ImportDeclaration";
}

export function getImportSourceValue(stmt: t.ImportDeclaration): string {
  return getStringValue(stmt.source);
}

export function getImportSourceRaw(
  sourceText: string,
  stmt: t.ImportDeclaration,
): string {
  return sourceText.slice(stmt.source.start, stmt.source.end);
}

export function getNamedImportSpecifiers(
  stmt: t.ImportDeclaration,
): NamedImportSpecifier[] {
  return stmt.specifiers.filter(isNamedImportSpecifier);
}

export function getNamedValueImportSpecifiers(
  stmt: t.ImportDeclaration,
): NamedImportSpecifier[] {
  if (isTypeOnlyImportDeclaration(stmt)) {
    return [];
  }

  return getNamedImportSpecifiers(stmt).filter(isValueImportSpecifier);
}

export function isValueImportSpecifier(
  specifier: NamedImportSpecifier,
): boolean {
  return specifier.importKind !== "type";
}

export function isTypeOnlyImportDeclaration(
  stmt: t.ImportDeclaration,
): boolean {
  return stmt.importKind === "type";
}

export function getImportedName(specifier: NamedImportSpecifier): string {
  return getStringValue(specifier.imported);
}

export function getLocalImportName(
  specifier: t.ImportDeclarationSpecifier,
): string {
  return getStringValue(specifier.local);
}

export function getImportSpecifierSource(
  sourceText: string,
  specifier: t.ImportDeclarationSpecifier,
): string {
  return sourceText.slice(specifier.start, specifier.end);
}

export function getStringValue(
  node: t.IdentifierName | t.StringLiteral,
): string {
  return node.type === "Identifier" ? node.name : node.value;
}

function isNamedImportSpecifier(
  specifier: t.ImportDeclarationSpecifier,
): specifier is NamedImportSpecifier {
  return specifier.type === "ImportSpecifier";
}
