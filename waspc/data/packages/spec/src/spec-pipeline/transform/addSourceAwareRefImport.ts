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
  REF_EXPORT_NAME,
  REF_IMPORT_FACTORY_EXPORT_NAME,
  SPEC_PACKAGE_NAME,
} from "./specPackageImports.js";

/**
 * Adds a local `ref` helper created with `_waspMakeRef(import.meta.url)`.
 *
 * Ref imports need the user's `.wasp.ts` file location. This rewrite makes
 * both handwritten and generated `ref(...)` calls use that location.
 */
export function addSourceAwareRefImport({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): {
  sourceText: string;
  refName: string;
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
    refName: plan.refName,
  };
}

type SourceAwareRefImportPlan = {
  refName: string;
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

  const importedRefName = getLocalNameForValueImport(
    specPackageImports,
    REF_EXPORT_NAME,
  );
  const refName = importedRefName ?? REF_EXPORT_NAME;
  const importEdits = removeRefFromSpecPackageImports({
    sourceFile,
    specPackageImports,
  });
  const helperEdit = addLocalRefHelper({
    anchorImport: firstSpecPackageImport,
    refName,
  });

  return {
    refName,
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

function removeRefFromSpecPackageImports({
  sourceFile,
  specPackageImports,
}: {
  sourceFile: ts.SourceFile;
  specPackageImports: ts.ImportDeclaration[];
}): Edit[] {
  const edits: Edit[] = [];

  for (const stmt of specPackageImports) {
    if (!importsRef(stmt)) {
      continue;
    }

    edits.push(removeRefFromSpecImport({ sourceFile, stmt }));
  }

  return edits;
}

function removeRefFromSpecImport({
  sourceFile,
  stmt,
}: {
  sourceFile: ts.SourceFile;
  stmt: ts.ImportDeclaration;
}): Edit {
  return replaceSpecPackageImportSpecifiers({
    sourceFile,
    stmt,
    nextSpecifiers: getSpecifiersWithoutRef(stmt),
  });
}

function importsRef(stmt: ts.ImportDeclaration): boolean {
  return Boolean(
    getNamedValueImports(stmt)?.elements.some((specifier) =>
      isValueImportSpecifierFor(specifier, REF_EXPORT_NAME),
    ),
  );
}

function getSpecifiersWithoutRef(
  stmt: ts.ImportDeclaration,
): ts.ImportSpecifier[] {
  return [...(getNamedValueImports(stmt)?.elements ?? [])].filter(
    (specifier) => !isValueImportSpecifierFor(specifier, REF_EXPORT_NAME),
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

function addLocalRefHelper({
  anchorImport,
  refName,
}: {
  anchorImport: ts.ImportDeclaration;
  refName: string;
}): Edit {
  return {
    start: anchorImport.getEnd(),
    end: anchorImport.getEnd(),
    text: `\n${getFactoryImportSource()}\nconst ${refName} = ${REF_IMPORT_FACTORY_EXPORT_NAME}(import.meta.url);`,
  };
}

function getFactoryImportSource(): string {
  return `import { ${REF_IMPORT_FACTORY_EXPORT_NAME} } from ${JSON.stringify(SPEC_PACKAGE_NAME)};`;
}
