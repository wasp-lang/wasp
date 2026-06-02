import { parseAst } from "rolldown/parseAst";
import type { ESTree as t } from "rolldown/utils";
import { DefaultRefObject, NamedRefObject } from "../../spec/refObject.js";
import { mapImportPath, RefImportPath } from "./mapImportPath.js";

export type PlannedImportReference =
  | { kind: "named"; refObject: NamedRefObject }
  | { kind: "default"; refObject: DefaultRefObject }
  | { kind: "namespace"; from: RefImportPath; alias: string };

export interface PlannedImport {
  references: PlannedImportReference[];
  removeImport: {
    start: number;
    end: number;
  };
}

/**
 * Given a Wasp Spec source file, returns a plan for replacing each
 * `with { type: "ref" }` import with inline RefObject consts. We call this
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

  return ast.body.filter(isRefImportDeclaration).map((node) => {
    const importSource = mapImportPath({
      refImportPath: getStringValue(node.source),
      importingFilePath,
      projectRootDir,
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
  importSource: RefImportPath,
  specifier: t.ImportDeclarationSpecifier,
): PlannedImportReference {
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

function getStringValue(node: t.IdentifierName | t.StringLiteral): string {
  return node.type === "Identifier" ? node.name : node.value;
}
