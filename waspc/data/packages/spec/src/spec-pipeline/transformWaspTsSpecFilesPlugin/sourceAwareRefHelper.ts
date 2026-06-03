import type { ESTree as t } from "rolldown/utils";
import type { Removal } from "./transformWaspTsSpecFile.js";
import {
  getImportWithoutRefSource,
  getLocalRefImportName,
  getSpecPackageImports,
  REF_EXPORT_NAME,
  REF_IMPORT_FACTORY_EXPORT_NAME,
  SPEC_PACKAGE_INTERNAL_NAME,
} from "./specPackageImports.js";
import { createUniqueTopLevelNameGenerator } from "./topLevelNameGenerator.js";

export type SourceAwareRefHelperPlan = {
  refName: string;
  source: string;
  removals: Removal[];
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
  const importedRefName = getLocalRefImportName(specPackageImports);
  const refName =
    importedRefName ?? nameGenerator.generateName(REF_EXPORT_NAME);
  const refFactoryName = nameGenerator.generateName(
    REF_IMPORT_FACTORY_EXPORT_NAME,
  );
  const specImportRewrites = getSpecPackageRefImportRewrites({
    sourceText,
    specPackageImports,
  });

  return {
    refName,
    source: [
      getRefFactoryImportSource(refFactoryName),
      `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
      `const ${refName} = ${refFactoryName}(${JSON.stringify(sourceFileUrl)});`,
      ...specImportRewrites.map((rewrite) => rewrite.source),
      ``,
    ].join("\n"),
    removals: specImportRewrites.map((rewrite) => rewrite.remove),
  };
}

function getSpecPackageRefImportRewrites({
  sourceText,
  specPackageImports,
}: {
  sourceText: string;
  specPackageImports: t.ImportDeclaration[];
}): { source: string; remove: Removal }[] {
  return specPackageImports.flatMap((stmt) => {
    if (!importsPublicRef(stmt)) {
      return [];
    }

    return [
      {
        source: getImportWithoutRefSource(sourceText, stmt) ?? "",
        remove: {
          start: stmt.start,
          end: stmt.end,
        },
      },
    ];
  });
}

function importsPublicRef(stmt: t.ImportDeclaration): boolean {
  return getLocalRefImportName([stmt]) !== undefined;
}

function getRefFactoryImportSource(refFactoryName: string): string {
  const importSpecifier =
    refFactoryName === REF_IMPORT_FACTORY_EXPORT_NAME
      ? REF_IMPORT_FACTORY_EXPORT_NAME
      : `${REF_IMPORT_FACTORY_EXPORT_NAME} as ${refFactoryName}`;

  return `import { ${importSpecifier} } from ${JSON.stringify(SPEC_PACKAGE_INTERNAL_NAME)};`;
}
