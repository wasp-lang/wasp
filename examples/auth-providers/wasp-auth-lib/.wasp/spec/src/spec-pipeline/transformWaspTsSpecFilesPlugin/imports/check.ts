import type { ESTree as t } from "rolldown/utils";
import { WaspSpecUserError } from "../../../spec/waspSpecUserError.js";
import { getStringValue } from "../util.js";

export function assertCanTransformImports(ast: t.Program): void {
  const hasRefExports = ast.body.some(isRefExportDeclaration);
  if (hasRefExports) {
    throw new WaspSpecUserError(
      "Re-exporting refs is not supported. First import the reference and then re-export it if needed.",
    );
  }
}

function isRefExportDeclaration(node: t.Statement): boolean {
  return (
    (node.type === "ExportNamedDeclaration" ||
      node.type === "ExportAllDeclaration") &&
    node.attributes.some(
      (attr) =>
        getStringValue(attr.key) === "type" &&
        getStringValue(attr.value) === "ref",
    )
  );
}
