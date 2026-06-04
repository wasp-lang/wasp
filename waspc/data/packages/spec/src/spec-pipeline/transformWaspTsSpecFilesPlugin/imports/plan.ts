import * as path from "node:path";
import type { ESTree as t } from "rolldown/utils";
import {
  getStringValue,
  getTopLevelBindings,
  makeSafeName,
  PUBLIC_REF_HELPER_IMPORT_NAME,
} from "../util.js";

import type {
  DefaultRefObjectDescriptor,
  NamedRefObjectDescriptor,
} from "@wasp.sh/spec";

export type Plan = {
  refImports: RefImport[];
  safeRefHelperName: string;
};

export type RefImportReference =
  | { kind: "named"; refObject: NamedRefObjectDescriptor }
  | { kind: "default"; refObject: DefaultRefObjectDescriptor }
  | { kind: "namespace"; from: string; alias: string };

export interface RefImport {
  references: RefImportReference[];
  removeImport: {
    start: number;
    end: number;
  };
}

export function planTransformImports(
  ast: t.Program,
  { importingFilePath }: { importingFilePath: string },
): Plan {
  const refImports = findRefImports(ast, importingFilePath);

  const scope = getTopLevelBindings(ast);

  const safeRefHelperName = makeSafeName(PUBLIC_REF_HELPER_IMPORT_NAME, scope);

  return { refImports, safeRefHelperName };
}

function findRefImports(ast: t.Program, importingFilePath: string) {
  return ast.body.filter(isRefImportDeclaration).map((node) => {
    const importSource = mapImportPath({
      refImportPath: getStringValue(node.source),
      importingFilePath,
    });

    return {
      references: node.specifiers.map((specifier) =>
        makeRefObject(importSource, specifier),
      ),
      removeImport: { start: node.start, end: node.end },
    };
  });
}

function isRefImportDeclaration(
  node: t.Statement,
): node is t.ImportDeclaration {
  return (
    node.type === "ImportDeclaration" &&
    node.attributes.some(
      (attr) =>
        getStringValue(attr.key) === "type" &&
        getStringValue(attr.value) === "ref",
    )
  );
}

function makeRefObject(
  importSource: string,
  specifier: t.ImportDeclarationSpecifier,
): RefImportReference {
  switch (specifier.type) {
    case "ImportSpecifier":
      return {
        kind: "named",
        refObject: {
          import: getStringValue(specifier.imported),
          alias: getStringValue(specifier.local),
          from: importSource,
        },
      };
    case "ImportDefaultSpecifier":
      return {
        kind: "default",
        refObject: {
          importDefault: getStringValue(specifier.local),
          from: importSource,
        },
      };
    case "ImportNamespaceSpecifier":
      return {
        kind: "namespace",
        alias: getStringValue(specifier.local),
        from: importSource,
      };
    default:
      return specifier satisfies never;
  }
}

/**
 * Resolves a ref import path (as written in the importing spec file) to an
 * `@src/...` path, which is how Wasp references files in the app's `src/`
 * directory.
 */
function mapImportPath({
  refImportPath,
  importingFilePath,
}: {
  refImportPath: string;
  importingFilePath: string;
}): string {
  return path.resolve(path.dirname(importingFilePath), refImportPath);
}
