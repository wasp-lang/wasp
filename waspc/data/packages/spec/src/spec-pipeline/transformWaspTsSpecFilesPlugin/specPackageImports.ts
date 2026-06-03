import type { ESTree as t } from "rolldown/utils";
import {
  getImportSourceRaw,
  getImportSourceValue,
  getImportSpecifierSource,
  getImportedName,
  getLocalImportName,
  getNamedImportSpecifiers,
  getNamedValueImportSpecifiers,
  isImportDeclaration,
  isValueImportSpecifier,
  type NamedImportSpecifier,
} from "./importDeclarations.js";

export const SPEC_PACKAGE_NAME = "@wasp.sh/spec";
export const SPEC_PACKAGE_INTERNAL_NAME = "@wasp.sh/spec/internal";
export const REF_HELPER_EXPORT_NAME = "ref";
export const MAKE_REF_EXPORT_NAME = "_waspMakeRef";

export function getSpecPackageImports(
  program: t.Program,
): t.ImportDeclaration[] {
  return program.body.filter(isImportDeclaration).filter(isSpecPackageImport);
}

export function getLocalRefHelperName(
  imports: t.ImportDeclaration[],
): string | undefined {
  for (const stmt of imports) {
    const refHelperSpecifier =
      getNamedValueImportSpecifiers(stmt).find(importsRefHelper);

    if (refHelperSpecifier) {
      return getLocalImportName(refHelperSpecifier);
    }
  }

  return undefined;
}

export function getImportWithoutRefHelperSource(
  sourceText: string,
  stmt: t.ImportDeclaration,
): string | undefined {
  if (stmt.specifiers.length === 0) {
    return undefined;
  }

  const defaultImport = getDefaultImportSource(sourceText, stmt);
  const namedImports = getNamedImportSpecifiers(stmt)
    .filter((specifier) => !isValueRefHelperSpecifier(specifier))
    .map((specifier) => getImportSpecifierSource(sourceText, specifier));

  const importParts = [
    defaultImport,
    namedImports.length > 0 ? `{ ${namedImports.join(", ")} }` : undefined,
  ].filter((part): part is string => part !== undefined);

  return importParts.length > 0
    ? `import ${importParts.join(", ")} from ${getImportSourceRaw(sourceText, stmt)};\n`
    : undefined;
}

function isSpecPackageImport(stmt: t.ImportDeclaration): boolean {
  return getImportSourceValue(stmt) === SPEC_PACKAGE_NAME;
}

function getDefaultImportSource(
  sourceText: string,
  stmt: t.ImportDeclaration,
): string | undefined {
  const specifier = stmt.specifiers.find(
    (specifier) => specifier.type === "ImportDefaultSpecifier",
  );

  return specifier
    ? getImportSpecifierSource(sourceText, specifier)
    : undefined;
}

function isValueRefHelperSpecifier(specifier: NamedImportSpecifier): boolean {
  return isValueImportSpecifier(specifier) && importsRefHelper(specifier);
}

function importsRefHelper(specifier: NamedImportSpecifier): boolean {
  return getImportedName(specifier) === REF_HELPER_EXPORT_NAME;
}
