import * as ts from "typescript";
import { SpecUserError } from "../../spec/specUserError.js";
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
}: {
  sourceText: string;
  sourcePath: string;
}): {
  sourceText: string;
  refImportName: string;
} {
  const sourceFile = parseSourceFile({ sourceText, sourcePath });
  const plan = getSourceAwareRefImportPlan(sourceFile);

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

function getSourceAwareRefImportPlan(
  sourceFile: ts.SourceFile,
): SourceAwareRefImportPlan {
  const specImports = getSpecApiImports(sourceFile);
  if (specImports.length === 0) {
    throw new SpecUserError(
      `Could not add a source-aware ref import helper because ${sourceFile.fileName} does not import from ${JSON.stringify(PACKAGE_SPEC_MODULE_NAME)}.`,
    );
  }

  const existingRefImportName = findValueImportLocalName(
    specImports,
    REF_IMPORT_NAME,
  );
  const existingMakeRefImportName = findValueImportLocalName(
    specImports,
    MAKE_REF_IMPORT_NAME,
  );

  const refImportName = existingRefImportName ?? REF_IMPORT_NAME;
  const makeRefImportName = existingMakeRefImportName ?? MAKE_REF_IMPORT_NAME;
  const helperInsertionOffset = getRefImportHelperInsertionOffset({
    specImports,
    existingMakeRefImportName,
  });
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
    insertionOffset: helperInsertionOffset,
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
  insertionOffset: number | undefined;
  shouldAddMakeRefImport: boolean;
};

function getRefImportHelperEdits({
  sourceFile,
  refImportName,
  makeRefImportName,
  insertionOffset,
  shouldAddMakeRefImport,
}: HelperEditOptions): Edit[] {
  const helperDeclaration = `const ${refImportName} = ${makeRefImportName}(import.meta.url);`;

  if (insertionOffset !== undefined) {
    const makeRefImportImport = shouldAddMakeRefImport
      ? getMakeRefImportImportSource()
      : "";

    return [
      {
        start: insertionOffset,
        end: insertionOffset,
        text: `\n${makeRefImportImport}${helperDeclaration}`,
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

function getRefImportHelperInsertionOffset({
  specImports,
  existingMakeRefImportName,
}: {
  specImports: ts.ImportDeclaration[];
  existingMakeRefImportName: string | undefined;
}): number | undefined {
  const anchorImport = existingMakeRefImportName
    ? findSpecImportWithValueImport(specImports, MAKE_REF_IMPORT_NAME)
    : specImports[0];

  return anchorImport?.getEnd();
}

function findSpecImportWithValueImport(
  specImports: ts.ImportDeclaration[],
  exportedName: string,
): ts.ImportDeclaration | undefined {
  return specImports.find((stmt) => {
    const namedBindings = getNamedValueImports(stmt);

    return namedBindings?.elements.some((specifier) =>
      isValueImportOf(specifier, exportedName),
    );
  });
}

function getNamedValueImports(
  stmt: ts.ImportDeclaration,
): ts.NamedImports | undefined {
  const importClause = stmt.importClause;
  if (!importClause || importClause.isTypeOnly) {
    return undefined;
  }

  const namedBindings = importClause.namedBindings;
  return namedBindings && ts.isNamedImports(namedBindings)
    ? namedBindings
    : undefined;
}

function getLastImportDeclarationEnd(
  sourceFile: ts.SourceFile,
): number | undefined {
  return sourceFile.statements.filter(ts.isImportDeclaration).at(-1)?.getEnd();
}
