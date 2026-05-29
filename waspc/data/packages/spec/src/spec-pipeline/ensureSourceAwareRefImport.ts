import * as ts from "typescript";
import { applyEdits, type Edit } from "./sourceEdits.js";
import {
  findValueImportLocalName,
  formatSpecApiImport,
  getSpecApiImports,
  isValueImportOf,
  MAKE_REF_IMPORT_NAME,
  PACKAGE_SPEC_MODULE_NAME,
  REF_IMPORT_NAME,
} from "./specApiImports.js";

export type EnsureSourceAwareRefImportResult = {
  sourceText: string;
  refImportName: string;
};

/**
 * Rewrites explicit `refImport(...)` usage so the local helper is created with
 * `makeRefImport(import.meta.url)`.
 *
 * Ref imports need the user's `.wasp.ts` file location. This rewrite makes
 * both handwritten and generated `refImport(...)` calls use that location.
 */
export function ensureSourceAwareRefImport({
  sourceText,
  sourcePath,
  required,
  helperDeclarationInsertionOffset,
}: {
  sourceText: string;
  sourcePath: string;
  required: boolean;
  helperDeclarationInsertionOffset?: number;
}): EnsureSourceAwareRefImportResult {
  const sourceFile = parseSourceFile({ sourceText, sourcePath });
  const plan = getSourceAwareRefImportPlan({
    sourceFile,
    required,
    helperDeclarationInsertionOffset,
  });

  if (!plan) {
    return { sourceText, refImportName: REF_IMPORT_NAME };
  }

  return {
    sourceText: applyEdits(sourceText, plan.edits),
    refImportName: plan.refImportName,
  };
}

type SourceAwareRefImportPlan = {
  refImportName: string;
  edits: Edit[];
};

function parseSourceFile({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): ts.SourceFile {
  return ts.createSourceFile(
    sourcePath,
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );
}

function getSourceAwareRefImportPlan({
  sourceFile,
  required,
  helperDeclarationInsertionOffset,
}: {
  sourceFile: ts.SourceFile;
  required: boolean;
  helperDeclarationInsertionOffset?: number;
}): SourceAwareRefImportPlan | undefined {
  const specImports = getSpecApiImports(sourceFile);
  const existingRefImportName = findValueImportLocalName(
    specImports,
    REF_IMPORT_NAME,
  );
  const existingMakeRefImportName = findValueImportLocalName(
    specImports,
    MAKE_REF_IMPORT_NAME,
  );

  if (!required && !existingRefImportName) {
    return undefined;
  }

  const refImportName = existingRefImportName ?? REF_IMPORT_NAME;
  const makeRefImportName = existingMakeRefImportName ?? MAKE_REF_IMPORT_NAME;
  const specImportEdits = getSpecApiImportEdits({
    sourceFile,
    specImports,
    shouldRemoveRefImport: Boolean(existingRefImportName),
    shouldAddMakeRefImport: !existingMakeRefImportName,
  });
  const helperEdits = getRefImportHelperEdits({
    sourceFile,
    refImportName,
    makeRefImportName,
    helperDeclarationInsertionOffset,
    shouldAddMakeRefImport:
      !existingMakeRefImportName && specImportEdits.length === 0,
  });

  return {
    refImportName,
    edits: [...specImportEdits, ...helperEdits],
  };
}

type SpecApiImportEditOptions = {
  sourceFile: ts.SourceFile;
  specImports: ts.ImportDeclaration[];
  shouldRemoveRefImport: boolean;
  shouldAddMakeRefImport: boolean;
};

function getSpecApiImportEdits({
  sourceFile,
  specImports,
  shouldRemoveRefImport,
  shouldAddMakeRefImport,
}: SpecApiImportEditOptions): Edit[] {
  const edits: Edit[] = [];
  let addedMakeRefImport = false;

  for (const stmt of specImports) {
    const importClause = stmt.importClause;
    if (!importClause || importClause.isTypeOnly) {
      continue;
    }

    const namedBindings = importClause.namedBindings;
    if (!namedBindings || !ts.isNamedImports(namedBindings)) {
      continue;
    }

    const filteredSpecifiers = shouldRemoveRefImport
      ? namedBindings.elements.filter(
          (specifier) => !isValueImportOf(specifier, REF_IMPORT_NAME),
        )
      : [...namedBindings.elements];
    const shouldAddHere = shouldAddMakeRefImport && !addedMakeRefImport;
    const nextSpecifiers = shouldAddHere
      ? [...filteredSpecifiers, MAKE_REF_IMPORT_NAME]
      : filteredSpecifiers;

    if (
      nextSpecifiers.length === namedBindings.elements.length &&
      !shouldAddHere
    ) {
      continue;
    }

    if (shouldAddHere) {
      addedMakeRefImport = true;
    }
    edits.push({
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      text: formatSpecApiImport(sourceFile, stmt, nextSpecifiers),
    });
  }

  return edits;
}

type HelperEditOptions = {
  sourceFile: ts.SourceFile;
  refImportName: string;
  makeRefImportName: string;
  helperDeclarationInsertionOffset?: number;
  shouldAddMakeRefImport: boolean;
};

function getRefImportHelperEdits({
  sourceFile,
  refImportName,
  makeRefImportName,
  helperDeclarationInsertionOffset,
  shouldAddMakeRefImport,
}: HelperEditOptions): Edit[] {
  const helperDeclaration = `const ${refImportName} = ${makeRefImportName}(import.meta.url);`;

  if (helperDeclarationInsertionOffset !== undefined) {
    const makeRefImportImport = shouldAddMakeRefImport
      ? getMakeRefImportImportSource()
      : "";

    return [
      {
        start: helperDeclarationInsertionOffset,
        end: helperDeclarationInsertionOffset,
        text: `${makeRefImportImport}${helperDeclaration}\n`,
      },
    ];
  }

  if (shouldAddMakeRefImport) {
    return [
      {
        start: 0,
        end: 0,
        text: `${getMakeRefImportImportSource()}${helperDeclaration}\n`,
      },
    ];
  }

  const insertAfter = getLastImportDeclarationEnd(sourceFile);

  if (insertAfter === undefined) {
    return [
      {
        start: 0,
        end: 0,
        text: `${helperDeclaration}\n`,
      },
    ];
  }

  return [
    {
      start: insertAfter,
      end: insertAfter,
      text: `\n${helperDeclaration}`,
    },
  ];
}

function getMakeRefImportImportSource(): string {
  return `import { ${MAKE_REF_IMPORT_NAME} } from ${JSON.stringify(PACKAGE_SPEC_MODULE_NAME)};\n`;
}

function getLastImportDeclarationEnd(
  sourceFile: ts.SourceFile,
): number | undefined {
  return sourceFile.statements.filter(ts.isImportDeclaration).at(-1)?.getEnd();
}
