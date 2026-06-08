import type { ESTree as t } from "rolldown/utils";
import { SpecUserError } from "../../../spec/specUserError.js";
import { PUBLIC_REF_HELPER_IMPORT_SOURCE } from "../util.js";

export function assertCanTransformRefHelper(ast: t.Program): void {
  const hasRefHelperExports = ast.body.some(isRefHelperExportDeclaration);
  if (hasRefHelperExports) {
    throw new SpecUserError(
      `Re-exporting from the ${PUBLIC_REF_HELPER_IMPORT_SOURCE} package is not supported. First import it and then re-export it if needed.`,
    );
  }
}

function isRefHelperExportDeclaration(node: t.Statement): boolean {
  return (
    (node.type === "ExportNamedDeclaration" ||
      node.type === "ExportAllDeclaration") &&
    node.source?.value === PUBLIC_REF_HELPER_IMPORT_SOURCE
  );
}
