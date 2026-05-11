import * as ts from "typescript";
import type { Result } from "../result.js";
import type { ExtImport } from "../spec/extImport.js";

const SRC_IMPORT_PREFIX = "@src/";

export type ImportLoweringResult = Result<
  ImportLoweringPlan,
  ImportDiagnostic[]
>;

export type ImportLoweringPlan = {
  replacements: ImportReplacement[];
};

export type ImportReplacement = {
  start: number;
  end: number;
  bindings: LoweredImportBinding[];
};

export type LoweredImportBinding =
  | {
      kind: "extImport";
      localName: string;
      extImport: ExtImport;
    }
  | {
      kind: "namespace";
      localName: string;
      from: ExtImport["from"];
      aliasPrefix: string;
    };

export type ImportDiagnostic = {
  unsupportedImportType: UnsupportedImportType;
  specifier: string;
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

export type SourceLocation = {
  line: number;
  column: number;
};

export function planImportLowering(sourceText: string): ImportLoweringResult {
  const sourceFile = ts.createSourceFile(
    "input.ts",
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );

  const replacements: ImportReplacement[] = [];
  const diagnostics: ImportDiagnostic[] = [];

  for (const stmt of sourceFile.statements) {
    const diagnostic = getUnsupportedSrcImportDiagnostic(sourceFile, stmt);
    if (diagnostic) {
      diagnostics.push(diagnostic);
      continue;
    }

    const replacement = getImportReplacement(sourceFile, stmt);
    if (replacement) {
      replacements.push(replacement);
    }
  }

  if (diagnostics.length > 0) {
    return { status: "error", error: diagnostics };
  }

  return { status: "ok", value: { replacements } };
}

function getUnsupportedSrcImportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.Statement,
): ImportDiagnostic | undefined {
  if (ts.isImportEqualsDeclaration(stmt)) {
    return getImportEqualsDiagnostic(sourceFile, stmt);
  }

  if (ts.isExportDeclaration(stmt)) {
    return getSrcReExportDiagnostic(sourceFile, stmt);
  }

  if (ts.isImportDeclaration(stmt)) {
    return getSrcImportDiagnostic(sourceFile, stmt);
  }

  return undefined;
}

function getImportReplacement(
  sourceFile: ts.SourceFile,
  stmt: ts.Statement,
): ImportReplacement | undefined {
  if (!ts.isImportDeclaration(stmt)) {
    return undefined;
  }

  const specifier = getSrcImportSpecifier(stmt.moduleSpecifier);
  if (!specifier) {
    return undefined;
  }

  const clause = stmt.importClause;
  if (!clause) {
    return undefined;
  }

  return {
    start: stmt.getStart(sourceFile),
    end: stmt.getEnd(),
    bindings: getLoweredImportBindings(clause, toExtImportPath(specifier)),
  };
}

function getSrcImportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
): ImportDiagnostic | undefined {
  const specifier = getSrcImportSpecifier(stmt.moduleSpecifier);
  if (!specifier) {
    return undefined;
  }

  const clause = stmt.importClause;
  if (!clause) {
    return makeDiagnostic(sourceFile, stmt, specifier, "sideEffect");
  }

  if (clause.isTypeOnly) {
    return makeDiagnostic(sourceFile, stmt, specifier, "typeOnly");
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamedImports(bindings)) {
    if (!clause.name && bindings.elements.length === 0) {
      return makeDiagnostic(sourceFile, stmt, specifier, "emptyNamed");
    }

    if (bindings.elements.some((element) => element.isTypeOnly)) {
      return makeDiagnostic(sourceFile, stmt, specifier, "mixedTypeAndValue");
    }

    if (bindings.elements.some(hasStringLiteralImportedName)) {
      return makeDiagnostic(sourceFile, stmt, specifier, "stringLiteral");
    }
  }

  return undefined;
}

function getImportEqualsDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportEqualsDeclaration,
): ImportDiagnostic | undefined {
  const specifier = getImportEqualsSpecifier(stmt);
  if (!specifier || !isSrcImportSpecifier(specifier)) {
    return undefined;
  }

  return makeDiagnostic(sourceFile, stmt, specifier, "importEquals");
}

function getSrcReExportDiagnostic(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): ImportDiagnostic | undefined {
  if (!stmt.moduleSpecifier) {
    return undefined;
  }

  const specifier = getSrcImportSpecifier(stmt.moduleSpecifier);
  if (!specifier) {
    return undefined;
  }

  return makeDiagnostic(sourceFile, stmt, specifier, "reExport");
}

function getLoweredImportBindings(
  clause: ts.ImportClause,
  from: ExtImport["from"],
): LoweredImportBinding[] {
  const bindings: LoweredImportBinding[] = [];

  if (clause.name) {
    const name = clause.name.text;
    bindings.push({
      kind: "extImport",
      localName: name,
      extImport: { importDefault: name, from },
    });
  }

  const namedBindings = clause.namedBindings;
  if (namedBindings && ts.isNamespaceImport(namedBindings)) {
    const name = namedBindings.name.text;
    bindings.push({
      kind: "namespace",
      localName: name,
      from,
      aliasPrefix: getNamespaceAliasPrefix(name),
    });
  } else if (namedBindings) {
    for (const spec of namedBindings.elements) {
      const localName = spec.name.text;
      const importedName = getImportedName(spec);
      bindings.push({
        kind: "extImport",
        localName,
        extImport: {
          import: importedName,
          from,
          ...(importedName === localName ? {} : { alias: localName }),
        },
      });
    }
  }

  return bindings;
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

function getSrcImportSpecifier(
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

function makeDiagnostic(
  sourceFile: ts.SourceFile,
  node: ts.Node,
  specifier: string,
  unsupportedImportType: UnsupportedImportType,
): ImportDiagnostic {
  const { line, character } = sourceFile.getLineAndCharacterOfPosition(
    node.getStart(sourceFile),
  );
  return {
    unsupportedImportType,
    specifier,
    location: { line: line + 1, column: character + 1 },
  };
}

function getNamespaceAliasPrefix(namespaceName: string): string {
  // Namespace imports synthesize descriptors on property access. Prefix aliases
  // with the namespace name so `ops.archive` and `legacyOps.archive` do not
  // collide after lowering.
  return `${namespaceName}_`;
}

function getImportedName(spec: ts.ImportSpecifier): string {
  // In `import { exported as local }`, TS stores `exported` in `propertyName`
  // and `local` in `name`. For `import { local }`, `propertyName` is undefined.
  return spec.propertyName?.text ?? spec.name.text;
}

function toExtImportPath(specifier: string): ExtImport["from"] {
  return specifier as ExtImport["from"];
}
