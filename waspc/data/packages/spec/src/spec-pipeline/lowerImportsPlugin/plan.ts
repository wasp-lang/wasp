import { parseAst } from "rolldown/parseAst";
import type { ESTree as t } from "rolldown/utils";
import { SpecUserError } from "../../spec/specUserError.js";
import { mapImportPath } from "./mapImportPath.js";

export interface PlannedImportBinding {
  import: string;
  alias: string;
}

export interface PlannedImport {
  from: string;
  bindings: PlannedImportBinding[];
  start: number;
  end: number;
}

/**
 * Given a Wasp Spec source file, returns a plan for replacing each
 * `with { type: "ref" }` import with inline ExtImport consts. We call this
 * lowering imports.
 */
export function planLowerImports({
  sourceText,
  importingFilePath,
  projectRootDir,
}: {
  sourceText: string;
  importingFilePath: string;
  projectRootDir: string;
}): PlannedImport[] {
  const ast = parseAst(sourceText, { lang: "ts" });

  return ast.body.filter(isRefImportDeclaration).map((node) => ({
    from: mapImportPath({
      refImportPath: getStringValue(node.source),
      importingFilePath,
      projectRootDir,
    }),
    bindings: node.specifiers.map(getSpecifierBinding),
    start: node.start,
    end: node.end,
  }));
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

function getSpecifierBinding(
  specifier: t.ImportDeclarationSpecifier,
): PlannedImportBinding {
  switch (specifier.type) {
    case "ImportSpecifier":
      return {
        import: getStringValue(specifier.imported),
        alias: getStringValue(specifier.local),
      };
    case "ImportDefaultSpecifier":
      return {
        import: "default",
        alias: getStringValue(specifier.local),
      };
    case "ImportNamespaceSpecifier":
      throw new SpecUserError(
        "Namespace ref imports (`import * as X`) are not supported.",
      );
    default:
      return specifier satisfies never;
  }
}

function getStringValue(node: t.IdentifierName | t.StringLiteral): string {
  return node.type === "Identifier" ? node.name : node.value;
}
