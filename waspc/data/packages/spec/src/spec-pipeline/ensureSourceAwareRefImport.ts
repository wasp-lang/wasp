import * as ts from "typescript";

const PACKAGE_SPEC_MODULE_NAME = "@wasp.sh/spec";
const REF_IMPORT_NAME = "refImport";
const MAKE_REF_IMPORT_NAME = "makeRefImport";

export type EnsureSourceAwareRefImportResult = {
  sourceText: string;
  refImportName?: string;
};

export function ensureSourceAwareRefImport({
  sourceText,
  sourcePath,
  required,
  insertBefore,
}: {
  sourceText: string;
  sourcePath: string;
  required: boolean;
  insertBefore?: number;
}): EnsureSourceAwareRefImportResult {
  const sourceFile = ts.createSourceFile(
    sourcePath,
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );
  const importDeclarations = sourceFile.statements.filter(
    ts.isImportDeclaration,
  );
  const specImports = importDeclarations.filter(isSpecImportDeclaration);
  const refImportName = findValueImportLocalName(specImports, REF_IMPORT_NAME);
  const importedMakeRefImportName = findValueImportLocalName(
    specImports,
    MAKE_REF_IMPORT_NAME,
  );

  if (!required && !refImportName) {
    return { sourceText };
  }

  const helperName = refImportName ?? REF_IMPORT_NAME;
  const makeRefImportName = importedMakeRefImportName ?? MAKE_REF_IMPORT_NAME;
  const replacements = getSpecImportReplacements({
    sourceFile,
    specImports,
    shouldRemoveRefImport: Boolean(refImportName),
    shouldAddMakeRefImport: !importedMakeRefImportName,
  });
  const insertions = getHelperInsertions({
    sourceFile,
    helperName,
    makeRefImportName,
    insertBefore,
    shouldAddMakeRefImport:
      !importedMakeRefImportName && replacements.length === 0,
  });
  const transformedSource = applyEdits(sourceText, [
    ...replacements,
    ...insertions,
  ]);

  return { sourceText: transformedSource, refImportName: helperName };
}

type SpecImportReplacementOptions = {
  sourceFile: ts.SourceFile;
  specImports: ts.ImportDeclaration[];
  shouldRemoveRefImport: boolean;
  shouldAddMakeRefImport: boolean;
};

function getSpecImportReplacements({
  sourceFile,
  specImports,
  shouldRemoveRefImport,
  shouldAddMakeRefImport,
}: SpecImportReplacementOptions): Edit[] {
  const replacements: Edit[] = [];
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
    const shouldAddHere: boolean =
      shouldAddMakeRefImport && !addedMakeRefImport;
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
    replacements.push({
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      text: formatSpecImport(sourceFile, stmt, nextSpecifiers),
    });
  }

  return replacements;
}

type HelperInsertionOptions = {
  sourceFile: ts.SourceFile;
  helperName: string;
  makeRefImportName: string;
  insertBefore?: number;
  shouldAddMakeRefImport: boolean;
};

function getHelperInsertions({
  sourceFile,
  helperName,
  makeRefImportName,
  insertBefore,
  shouldAddMakeRefImport,
}: HelperInsertionOptions): Edit[] {
  const helperDeclaration = `const ${helperName} = ${makeRefImportName}(import.meta.url);`;

  if (insertBefore !== undefined) {
    const makeRefImportImport = shouldAddMakeRefImport
      ? `import { ${MAKE_REF_IMPORT_NAME} } from ${JSON.stringify(PACKAGE_SPEC_MODULE_NAME)};\n`
      : "";

    return [
      {
        start: insertBefore,
        end: insertBefore,
        text: `${makeRefImportImport}${helperDeclaration}\n`,
      },
    ];
  }

  if (shouldAddMakeRefImport) {
    return [
      {
        start: 0,
        end: 0,
        text:
          `import { ${MAKE_REF_IMPORT_NAME} } from ${JSON.stringify(PACKAGE_SPEC_MODULE_NAME)};\n` +
          `${helperDeclaration}\n`,
      },
    ];
  }

  const lastImportDeclaration = sourceFile.statements
    .filter(ts.isImportDeclaration)
    .at(-1);

  if (!lastImportDeclaration) {
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
      start: lastImportDeclaration.getEnd(),
      end: lastImportDeclaration.getEnd(),
      text: `\n${helperDeclaration}`,
    },
  ];
}

type ImportSpecifierSource = ts.ImportSpecifier | string;

function formatSpecImport(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  namedSpecifiers: ImportSpecifierSource[],
): string {
  const importClause = stmt.importClause;
  if (!importClause) {
    return "";
  }

  const defaultImport = importClause.name?.text;
  const namedImports = namedSpecifiers.map((specifier) =>
    typeof specifier === "string" ? specifier : specifier.getText(sourceFile),
  );

  if (!defaultImport && namedImports.length === 0) {
    return "";
  }

  const importParts = [
    defaultImport,
    namedImports.length > 0 ? `{ ${namedImports.join(", ")} }` : undefined,
  ].filter((part): part is string => part !== undefined);

  return `import ${importParts.join(", ")} from ${stmt.moduleSpecifier.getText(sourceFile)};`;
}

function findValueImportLocalName(
  imports: ts.ImportDeclaration[],
  exportedName: string,
): string | undefined {
  for (const stmt of imports) {
    const importClause = stmt.importClause;
    if (!importClause || importClause.isTypeOnly) {
      continue;
    }

    const namedBindings = importClause.namedBindings;
    if (!namedBindings || !ts.isNamedImports(namedBindings)) {
      continue;
    }

    const specifier = namedBindings.elements.find((specifier) =>
      isValueImportOf(specifier, exportedName),
    );
    if (specifier) {
      return specifier.name.text;
    }
  }

  return undefined;
}

function isValueImportOf(
  specifier: ts.ImportSpecifier,
  exportedName: string,
): boolean {
  return !specifier.isTypeOnly && getImportedName(specifier) === exportedName;
}

function getImportedName(specifier: ts.ImportSpecifier): string {
  return specifier.propertyName?.text ?? specifier.name.text;
}

function isSpecImportDeclaration(stmt: ts.ImportDeclaration): boolean {
  return (
    ts.isStringLiteral(stmt.moduleSpecifier) &&
    isSpecModuleSpecifier(stmt.moduleSpecifier.text)
  );
}

function isSpecModuleSpecifier(moduleSpecifier: string): boolean {
  return (
    moduleSpecifier === PACKAGE_SPEC_MODULE_NAME ||
    /\/src\/spec\/publicApi\/index\.[jt]s$/.test(moduleSpecifier)
  );
}

type Edit = {
  start: number;
  end: number;
  text: string;
};

function applyEdits(sourceText: string, edits: Edit[]): string {
  const sortedEdits = [...edits].sort(
    (a, b) => a.start - b.start || a.end - b.end,
  );
  let sourceCursor = 0;
  let modifiedSource = "";

  for (const edit of sortedEdits) {
    modifiedSource += sourceText.slice(sourceCursor, edit.start);
    modifiedSource += edit.text;
    sourceCursor = edit.end;
  }

  modifiedSource += sourceText.slice(sourceCursor);

  return modifiedSource;
}
