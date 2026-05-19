import * as ts from "typescript";

const SRC_IMPORT_PREFIX = "@src/";

export type ImportDiagnostic = {
  unsupportedImportType: UnsupportedImportType;
  specifier: string;
  filePath: string;
  location: SourceLocation;
};

export type UnsupportedImportType =
  // Example: `import "@src/setup";`
  | "sideEffect"
  // Example: `import MainPage = require("@src/MainPage");`
  | "importEquals"
  // Example: `import type { Props } from "@src/MainPage";`
  | "typeOnly"
  // Example: `import { type Props, MainPage } from "@src/MainPage";`
  | "mixedTypeAndValue"
  // Example: `import { "foo-bar" as fooBar } from "@src/operations";`
  | "stringLiteral"
  // Example: `import {} from "@src/MainPage";`
  | "emptyNamed"
  // Example: `export { MainPage } from "@src/MainPage";`
  | "reExport";

type SourceLocation = {
  line: number;
  column: number;
};

export class DiagnosticError extends Error {
  readonly diagnostic: ImportDiagnostic;

  constructor(
    sourceFile: ts.SourceFile,
    node: ts.Node,
    specifier: string,
    unsupportedImportType: UnsupportedImportType,
  ) {
    super("Import diagnostic");

    const { line, character } = sourceFile.getLineAndCharacterOfPosition(
      node.getStart(sourceFile),
    );
    this.diagnostic = {
      unsupportedImportType,
      specifier,
      filePath: sourceFile.fileName,
      location: { line: line + 1, column: character + 1 },
    };
  }
}

/**
 * Supported @src imports are value imports, e.g.:
 * `import MainPage from "@src/MainPage"`,
 * `import { getTasks } from "@src/operations"`, or
 * `import * as ops from "@src/operations"`.
 */
export function assertSupportedSrcImportDeclaration(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  specifier: string,
): asserts stmt is ts.ImportDeclaration & { importClause: ts.ImportClause } {
  const clause = stmt.importClause;
  if (!clause) {
    throw new DiagnosticError(sourceFile, stmt, specifier, "sideEffect");
  }

  if (clause.isTypeOnly) {
    throw new DiagnosticError(sourceFile, stmt, specifier, "typeOnly");
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamedImports(bindings)) {
    if (!clause.name && bindings.elements.length === 0) {
      throw new DiagnosticError(sourceFile, stmt, specifier, "emptyNamed");
    }

    if (bindings.elements.some((element) => element.isTypeOnly)) {
      throw new DiagnosticError(
        sourceFile,
        stmt,
        specifier,
        "mixedTypeAndValue",
      );
    }

    if (bindings.elements.some(hasStringLiteralImportedName)) {
      throw new DiagnosticError(sourceFile, stmt, specifier, "stringLiteral");
    }
  }
}

/**
 * Import-equals declarations are supported unless they target @src, e.g.
 * `import path = require("node:path")`.
 */
export function assertNonSrcImportEqualsDeclaration(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportEqualsDeclaration,
): void {
  const specifier = getImportEqualsSpecifier(stmt);
  if (!specifier || !isSrcImportSpecifier(specifier)) {
    return;
  }

  throw new DiagnosticError(sourceFile, stmt, specifier, "importEquals");
}

/**
 * Re-exports are supported unless they target @src, e.g.
 * `export { helper } from "./helpers"`.
 */
export function assertNonSrcReExport(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): void {
  if (!stmt.moduleSpecifier) {
    return;
  }

  const specifier = getSrcImportSpecifier(stmt.moduleSpecifier);
  if (!specifier) {
    return;
  }

  throw new DiagnosticError(sourceFile, stmt, specifier, "reExport");
}

function hasStringLiteralImportedName(spec: ts.ImportSpecifier): boolean {
  return (
    spec.propertyName !== undefined && ts.isStringLiteral(spec.propertyName)
  );
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

export function getSrcImportSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  const specifier = getStringModuleSpecifier(moduleSpecifier);
  if (!specifier || !isSrcImportSpecifier(specifier)) {
    return undefined;
  }

  return specifier;
}

function getStringModuleSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  return ts.isStringLiteral(moduleSpecifier) ? moduleSpecifier.text : undefined;
}

function isSrcImportSpecifier(specifier: string): boolean {
  return specifier.startsWith(SRC_IMPORT_PREFIX);
}
