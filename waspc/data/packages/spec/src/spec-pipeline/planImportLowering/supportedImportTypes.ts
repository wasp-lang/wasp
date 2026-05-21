import * as ts from "typescript";

export type ImportDiagnostic = {
  unsupportedImportType: UnsupportedImportType;
  refImportPath: string;
  filePath: string;
  location: SourceLocation;
};

export type UnsupportedImportType =
  // Example: `import "./src/setup" with { type: "ref" };`
  | "sideEffect"
  // Example: `import type { Props } from "./src/MainPage" with { type: "ref" };`
  | "typeOnly"
  // Example: `import { type Props, MainPage } from "./src/MainPage" with { type: "ref" };`
  | "mixedTypeAndValue"
  // Example: `import { "foo-bar" as fooBar } from "./src/operations" with { type: "ref" };`
  | "stringLiteral"
  // Example: `import {} from "./src/MainPage" with { type: "ref" };`
  | "emptyNamed"
  // Example: `export { MainPage } from "./src/MainPage" with { type: "ref" };`
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
    refImportPath: string,
    unsupportedImportType: UnsupportedImportType,
  ) {
    super("Import diagnostic");

    const { line, character } = sourceFile.getLineAndCharacterOfPosition(
      node.getStart(sourceFile),
    );
    this.diagnostic = {
      unsupportedImportType,
      refImportPath,
      filePath: sourceFile.fileName,
      location: { line: line + 1, column: character + 1 },
    };
  }
}

/**
 * Supported ref imports are value imports, e.g.:
 * `import MainPage from "./src/MainPage" with { type: "ref" }`,
 * `import { getTasks } from "./src/operations" with { type: "ref" }`, or
 * `import * as ops from "./src/operations" with { type: "ref" }`.
 */
export function assertSupportedRefImportDeclaration(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  refImportPath: string,
): asserts stmt is ts.ImportDeclaration & { importClause: ts.ImportClause } {
  const clause = stmt.importClause;
  if (!clause) {
    throw new DiagnosticError(sourceFile, stmt, refImportPath, "sideEffect");
  }

  if (clause.isTypeOnly) {
    throw new DiagnosticError(sourceFile, stmt, refImportPath, "typeOnly");
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamedImports(bindings)) {
    if (!clause.name && bindings.elements.length === 0) {
      throw new DiagnosticError(sourceFile, stmt, refImportPath, "emptyNamed");
    }

    if (bindings.elements.some((element) => element.isTypeOnly)) {
      throw new DiagnosticError(
        sourceFile,
        stmt,
        refImportPath,
        "mixedTypeAndValue",
      );
    }

    if (bindings.elements.some(hasStringLiteralImportedName)) {
      throw new DiagnosticError(
        sourceFile,
        stmt,
        refImportPath,
        "stringLiteral",
      );
    }
  }
}

/**
 * Re-exports should not be marked as refs.
 */
export function assertNonRefReExport(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): void {
  const refImportPath = getRefExportSpecifier(stmt);
  if (!refImportPath) {
    return;
  }

  throw new DiagnosticError(sourceFile, stmt, refImportPath, "reExport");
}

export function getRefImportSpecifier(
  stmt: ts.ImportDeclaration,
): string | undefined {
  if (!hasOnlyRefImportAttribute(stmt.attributes)) {
    return undefined;
  }

  return getStringModuleSpecifier(stmt.moduleSpecifier);
}

function getRefExportSpecifier(stmt: ts.ExportDeclaration): string | undefined {
  if (!stmt.moduleSpecifier || !hasOnlyRefImportAttribute(stmt.attributes)) {
    return undefined;
  }

  return getStringModuleSpecifier(stmt.moduleSpecifier);
}

function hasStringLiteralImportedName(spec: ts.ImportSpecifier): boolean {
  return (
    spec.propertyName !== undefined && ts.isStringLiteral(spec.propertyName)
  );
}

/**
 * Detects if the given attributes indicate a ref import e.g.
 *
 * import X from "..." with { type: "ref" };
 */
function hasOnlyRefImportAttribute(
  attributes: ts.ImportAttributes | undefined,
): boolean {
  if (
    !attributes ||
    attributes.token !== ts.SyntaxKind.WithKeyword ||
    attributes.elements.length !== 1
  ) {
    return false;
  }

  const [attribute] = attributes.elements;
  return (
    attribute !== undefined &&
    attribute.name.text === "type" &&
    ts.isStringLiteral(attribute.value) &&
    attribute.value.text === "ref"
  );
}

function getStringModuleSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  return ts.isStringLiteral(moduleSpecifier) ? moduleSpecifier.text : undefined;
}
