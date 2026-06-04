import type { ESTree as t } from "rolldown/utils";
import {
  getStringValue,
  getTopLevelBindings,
  INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
  makeSafeName,
  PUBLIC_REF_HELPER_IMPORT_NAME,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";

export type Plan = {
  refHelperImports: t.ImportSpecifier[];
  safeInternalHelperName: string;
  safeRefHelperName: string;
};

export function planTransformRefHelper(ast: t.Program): Plan {
  const refHelperImports = findRefHelperImports(ast);

  const scope = getTopLevelBindings(ast);

  const safeInternalHelperName = makeSafeName(
    INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
    scope,
  );

  const safeRefHelperName = makeSafeName(PUBLIC_REF_HELPER_IMPORT_NAME, scope);

  return {
    refHelperImports,
    safeInternalHelperName,
    safeRefHelperName,
  };
}

function findRefHelperImports(ast: t.Program) {
  return ast.body
    .filter(
      (importNode): importNode is t.ImportDeclaration =>
        importNode.type === "ImportDeclaration" &&
        importNode.importKind === "value" &&
        getStringValue(importNode.source) === PUBLIC_REF_HELPER_IMPORT_SOURCE,
    )
    .flatMap((specifierNode) =>
      specifierNode.specifiers.filter(
        (node): node is t.ImportSpecifier =>
          node.type === "ImportSpecifier" &&
          node.importKind === "value" &&
          getStringValue(node.imported) === PUBLIC_REF_HELPER_IMPORT_NAME,
      ),
    );
}
