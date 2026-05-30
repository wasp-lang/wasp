import * as ts from "typescript";
import { SpecUserError } from "../../spec/specUserError.js";
import { applyEdits, type Edit } from "./sourceEdits.js";
import {
  formatSpecPackageImport,
  getLocalNameForValueImport,
  getNamedImports,
  getNamedValueImports,
  getSpecPackageImports,
  isImportSpecifierFor,
  isValueImportSpecifierFor,
  REF_IMPORT_EXPORT_NAME,
  REF_IMPORT_FACTORY_EXPORT_NAME,
  SPEC_PACKAGE_NAME,
} from "./specPackageImports.js";

/**
 * Adds a local `refImport` helper created with `_waspMakeRef(import.meta.url)`.
 *
 * Ref imports need the user's `.wasp.ts` file location. This rewrite makes
 * both handwritten and generated `refImport(...)` calls use that location.
 */
export function addSourceAwareRefImport({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): {
  sourceText: string;
  refImportName: string;
} {
  const sourceFile = ts.createSourceFile(
    sourcePath,
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );
  const plan = planSourceAwareRefImport(sourceFile);

  return {
    sourceText: applyEdits(sourceText, plan.edits),
    refImportName: plan.refImportName,
  };
}

type SourceAwareRefImportPlan = {
  refImportName: string;
  edits: Edit[];
};

function planSourceAwareRefImport(
  sourceFile: ts.SourceFile,
): SourceAwareRefImportPlan {
  const specPackageImports = getSpecPackageImports(sourceFile);
  const firstSpecPackageImport = specPackageImports[0];
  if (!firstSpecPackageImport) {
    throw new SpecUserError(
      `Could not add a source-aware ref import helper because ${sourceFile.fileName} does not import from ${JSON.stringify(SPEC_PACKAGE_NAME)}.`,
    );
  }

  assertInternalRefImportFactoryIsNotImported(specPackageImports);

  const importedRefImportName = getLocalNameForValueImport(
    specPackageImports,
    REF_IMPORT_EXPORT_NAME,
  );
  const refImportName = importedRefImportName ?? REF_IMPORT_EXPORT_NAME;
  const importEdits = removeRefImportFromSpecPackageImports({
    sourceFile,
    specPackageImports,
  });
  const helperEdit = addLocalRefImportHelper({
    anchorImport: firstSpecPackageImport,
    refImportName,
  });

  return {
    refImportName,
    edits: [...importEdits, helperEdit],
  };
}

function assertInternalRefImportFactoryIsNotImported(
  specPackageImports: ts.ImportDeclaration[],
): void {
  const importsRefImportFactory = specPackageImports.some((stmt) =>
    getNamedImports(stmt)?.elements.some((specifier) =>
      isImportSpecifierFor(specifier, REF_IMPORT_FACTORY_EXPORT_NAME),
    ),
  );

  if (importsRefImportFactory) {
    throw new SpecUserError(
      `The ${JSON.stringify(REF_IMPORT_FACTORY_EXPORT_NAME)} function is internal and must not be imported from ${JSON.stringify(SPEC_PACKAGE_NAME)}.`,
    );
  }
}

function removeRefImportFromSpecPackageImports({
  sourceFile,
  specPackageImports,
}: {
  sourceFile: ts.SourceFile;
  specPackageImports: ts.ImportDeclaration[];
}): Edit[] {
  const edits: Edit[] = [];

  for (const stmt of specPackageImports) {
    if (!importsRefImport(stmt)) {
      continue;
    }

    edits.push(removeRefImportFromSpecImport({ sourceFile, stmt }));
  }

  return edits;
}

function removeRefImportFromSpecImport({
  sourceFile,
  stmt,
}: {
  sourceFile: ts.SourceFile;
  stmt: ts.ImportDeclaration;
}): Edit {
  return replaceSpecPackageImportSpecifiers({
    sourceFile,
    stmt,
    nextSpecifiers: getSpecifiersWithoutRefImport(stmt),
  });
}

function importsRefImport(stmt: ts.ImportDeclaration): boolean {
  return Boolean(
    getNamedValueImports(stmt)?.elements.some((specifier) =>
      isValueImportSpecifierFor(specifier, REF_IMPORT_EXPORT_NAME),
    ),
  );
}

function getSpecifiersWithoutRefImport(
  stmt: ts.ImportDeclaration,
): ts.ImportSpecifier[] {
  return [...(getNamedValueImports(stmt)?.elements ?? [])].filter(
    (specifier) =>
      !isValueImportSpecifierFor(specifier, REF_IMPORT_EXPORT_NAME),
  );
}

function replaceSpecPackageImportSpecifiers({
  sourceFile,
  stmt,
  nextSpecifiers,
}: {
  sourceFile: ts.SourceFile;
  stmt: ts.ImportDeclaration;
  nextSpecifiers: (ts.ImportSpecifier | string)[];
}): Edit {
  return {
    start: stmt.getStart(sourceFile),
    end: stmt.getEnd(),
    text: formatSpecPackageImport(sourceFile, stmt, nextSpecifiers),
  };
}

function addLocalRefImportHelper({
  anchorImport,
  refImportName,
}: {
  anchorImport: ts.ImportDeclaration;
  refImportName: string;
}): Edit {
  return {
    start: anchorImport.getEnd(),
    end: anchorImport.getEnd(),
    text: `\n${getFactoryImportSource()}\nconst ${refImportName} = ${REF_IMPORT_FACTORY_EXPORT_NAME}(import.meta.url);`,
  };
}

function getFactoryImportSource(): string {
  return `import { ${REF_IMPORT_FACTORY_EXPORT_NAME} } from ${JSON.stringify(SPEC_PACKAGE_NAME)};`;
}
