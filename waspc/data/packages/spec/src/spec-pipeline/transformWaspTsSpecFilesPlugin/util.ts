import { createVisitors } from "estree-util-scope";
import { walk } from "estree-walker";
import assert from "node:assert/strict";
import { type ESTree as t } from "rolldown/utils";

export const PUBLIC_REF_HELPER_IMPORT_SOURCE = "@wasp.sh/spec";
export const PUBLIC_REF_HELPER_IMPORT_NAME = "ref";

export const INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE = "@wasp.sh/spec/internal";
export const INTERNAL_MAKE_REF_HELPER_IMPORT_NAME = "_waspMakeRef";

export function getStringValue(
  node: t.IdentifierName | t.StringLiteral,
): string {
  return node.type === "Identifier" ? node.name : node.value;
}

export function getTopLevelBindings(ast: t.Program): Set<string> {
  const visitors = createVisitors();

  walk(
    // @ts-expect-error - The types are not exactly the same between Rolldown
    // and ESTree, but the differences are in extra metadata and not the core
    // structure.
    ast,
    {
      enter(node) {
        visitors.enter(node);

        // Taken from estree-util-scope's documentation, we don't enter any
        // nodes that define their own scope, so we stay in the top-level.
        // https://github.com/syntax-tree/estree-util-scope#example-just-the-top-scope
        if (
          node.type === "ArrowFunctionExpression" ||
          node.type === "FunctionDeclaration" ||
          node.type === "FunctionExpression" ||
          node.type === "BlockStatement"
        ) {
          this.skip();
          visitors.exit(node);
        }
      },
      leave: visitors.exit,
    },
  );

  const topLevelScope = visitors.scopes.at(-1);
  assert(
    topLevelScope,
    "Expected to find a scope for the top-level program node",
  );

  const bindings = new Set(topLevelScope.defined);

  return bindings;
}

export function makeSafeName(
  desiredName: string,
  definedBindings: ReadonlySet<string>,
): string {
  let i = 0;
  let name = desiredName;
  while (definedBindings.has(name)) {
    name = `${desiredName}_${i++}`;
  }
  return name;
}

export function buildImportStatement(
  imports: readonly [importName: string, localName?: string][],
  source: string,
) {
  const specifiers = imports
    .map(([importName, localName]) =>
      !localName || importName === localName
        ? importName
        : `${importName} as ${localName}`,
    )
    .join(", ");

  return `import { ${specifiers} } from ${JSON.stringify(source)};\n`;
}
