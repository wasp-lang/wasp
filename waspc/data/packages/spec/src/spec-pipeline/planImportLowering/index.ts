import * as ts from "typescript";
import type { ReferenceObject } from "../../spec/referenceObject.js";
import { SpecUserError } from "../../spec/specUserError.js";
import type { LoweredImportBinding } from "./loweredImportBindings.js";
import { getLoweredImportBindings } from "./loweredImportBindings.js";
import type {
  ImportDiagnostic,
  UnsupportedImportType,
} from "./supportedImportTypes.js";
import {
  assertNonSrcImportEqualsDeclaration,
  assertNonSrcReExport,
  assertSupportedSrcImportDeclaration,
  DiagnosticError,
  getSrcImportSpecifier,
} from "./supportedImportTypes.js";

export type ImportLoweringPlan = {
  replacements: ImportReplacement[];
};

type ImportReplacement = {
  start: number;
  end: number;
  bindings: LoweredImportBinding[];
};

/**
 * Given source code, detects supported @src import statements and returns a
 * plan for replacing them with inline ReferenceObject consts. We call this
 * lowering imports.
 */
export function planImportLowering({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): ImportLoweringPlan {
  const sourceFile = ts.createSourceFile(
    sourcePath,
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );

  const replacements: ImportReplacement[] = [];
  const diagnostics: ImportDiagnostic[] = [];

  for (const stmt of sourceFile.statements) {
    try {
      replacements.push(...planStatementLowering(sourceFile, stmt));
    } catch (error) {
      if (!(error instanceof DiagnosticError)) {
        throw error;
      }
      diagnostics.push(error.diagnostic);
    }
  }

  if (diagnostics.length > 0) {
    throw new SpecUserError(formatImportDiagnostics(diagnostics));
  }

  return { replacements };
}

function planStatementLowering(
  sourceFile: ts.SourceFile,
  stmt: ts.Statement,
): ImportReplacement[] {
  if (ts.isImportEqualsDeclaration(stmt)) {
    assertNonSrcImportEqualsDeclaration(sourceFile, stmt);
    return [];
  }

  if (ts.isExportDeclaration(stmt)) {
    assertNonSrcReExport(sourceFile, stmt);
    return [];
  }

  if (!ts.isImportDeclaration(stmt)) {
    return [];
  }

  const specifier = getSrcImportSpecifier(stmt.moduleSpecifier);
  if (!specifier) {
    return [];
  }

  assertSupportedSrcImportDeclaration(sourceFile, stmt, specifier);
  return [
    {
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      bindings: getLoweredImportBindings(
        stmt.importClause,
        toReferencePath(specifier),
      ),
    },
  ];
}

function toReferencePath(specifier: string): ReferenceObject["from"] {
  return specifier as ReferenceObject["from"];
}

function formatImportDiagnostics(diagnostics: ImportDiagnostic[]): string {
  return [
    ...diagnostics.map(formatImportDiagnosticLine),
    "",
    "Supported @src imports are default, named, aliased named, or namespace imports from @src/*.",
  ].join("\n");
}

function formatImportDiagnosticLine(diagnostic: ImportDiagnostic): string {
  return `${diagnostic.filePath}(${diagnostic.location.line},${diagnostic.location.column}): error: Unsupported @src import ${JSON.stringify(diagnostic.specifier)}. ${formatUnsupportedImportType(diagnostic.unsupportedImportType)}`;
}

function formatUnsupportedImportType(type: UnsupportedImportType): string {
  switch (type) {
    case "sideEffect":
      return "Side-effect imports are not supported.";
    case "importEquals":
      return "Import equals declarations are not supported.";
    case "typeOnly":
      return "Type-only imports are not supported.";
    case "mixedTypeAndValue":
      return "Mixed type/value imports are not supported.";
    case "stringLiteral":
      return "String-literal named imports are not supported.";
    case "emptyNamed":
      return "Empty named imports are not supported.";
    case "reExport":
      return "Re-exports are not supported.";
  }
}
