import * as ts from "typescript";
import type { Result } from "../../result.js";
import type { ExtImport } from "../../spec/extImport.js";
import type { LoweredImportBinding } from "./loweredImportBindings.js";
import { getLoweredImportBindings } from "./loweredImportBindings.js";
import type { ImportDiagnostic } from "./supportedImportTypes.js";
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
 * plan for replacing them with inline ExtImport consts. We call this lowering
 * imports.
 */
export function planImportLowering({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): Result<ImportLoweringPlan, ImportDiagnostic[]> {
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
    return { status: "error", error: diagnostics };
  }

  return { status: "ok", value: { replacements } };
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
        toExtImportPath(specifier),
      ),
    },
  ];
}

function toExtImportPath(specifier: string): ExtImport["from"] {
  return specifier as ExtImport["from"];
}
