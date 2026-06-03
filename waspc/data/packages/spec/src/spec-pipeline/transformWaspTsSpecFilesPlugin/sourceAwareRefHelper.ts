import type { ESTree as t } from "rolldown/utils";
import {
  getImportWithoutRefHelperSource,
  getLocalRefHelperName,
  getSpecPackageImports,
  MAKE_REF_EXPORT_NAME,
  REF_HELPER_EXPORT_NAME,
  SPEC_PACKAGE_INTERNAL_NAME,
} from "./specPackageImports.js";
import { createUniqueTopLevelNameGenerator } from "./topLevelNameGenerator.js";
import type { SourceRemoval } from "./transformWaspTsSpecFile.js";

export type SourceAwareRefHelperPlan = {
  refHelperName: string;
  source: string;
  removals: SourceRemoval[];
};

export function planSourceAwareRefHelper({
  program,
  sourceText,
  sourceFileUrl,
}: {
  program: t.Program;
  sourceText: string;
  sourceFileUrl: string;
}): SourceAwareRefHelperPlan {
  const specPackageImports = getSpecPackageImports(program);
  const nameGenerator = createUniqueTopLevelNameGenerator(program);
  const importedRefHelperName = getLocalRefHelperName(specPackageImports);
  const refHelperName =
    importedRefHelperName ?? nameGenerator.generateName(REF_HELPER_EXPORT_NAME);
  const makeRefName = nameGenerator.generateName(MAKE_REF_EXPORT_NAME);
  const refHelperImportRewrites = getSpecPackageRefHelperImportRewrites({
    sourceText,
    specPackageImports,
  });

  return {
    refHelperName,
    source: [
      getMakeRefImportSource(makeRefName),
      `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
      `const ${refHelperName} = ${makeRefName}(${JSON.stringify(sourceFileUrl)});`,
      ...refHelperImportRewrites.map((rewrite) => rewrite.source),
      ``,
    ].join("\n"),
    removals: refHelperImportRewrites.map((rewrite) => rewrite.removal),
  };
}

function getMakeRefImportSource(makeRefName: string): string {
  const importSpecifier =
    makeRefName === MAKE_REF_EXPORT_NAME
      ? MAKE_REF_EXPORT_NAME
      : `${MAKE_REF_EXPORT_NAME} as ${makeRefName}`;

  return `import { ${importSpecifier} } from ${JSON.stringify(SPEC_PACKAGE_INTERNAL_NAME)};`;
}

function getSpecPackageRefHelperImportRewrites({
  sourceText,
  specPackageImports,
}: {
  sourceText: string;
  specPackageImports: t.ImportDeclaration[];
}): { source: string; removal: SourceRemoval }[] {
  return specPackageImports.flatMap((stmt) => {
    if (!importsRefHelper(stmt)) {
      return [];
    }

    return [
      {
        source: getImportWithoutRefHelperSource(sourceText, stmt) ?? "",
        removal: {
          start: stmt.start,
          end: stmt.end,
        },
      },
    ];
  });
}

function importsRefHelper(stmt: t.ImportDeclaration): boolean {
  return getLocalRefHelperName([stmt]) !== undefined;
}
