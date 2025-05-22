import type { RootContent } from "mdast";

/**
 * Imports in MDX should have their corresponding JS AST attached
 * This helper does it manually
 * (Structure taken from the
 * [MDX playground](https://mdxjs.com/playground/))
 */
export const makeImports = (
  imports: { from: string; importDefaultAs: string }[],
): RootContent => {
  return {
    type: "mdxjsEsm",
    value: imports
      .map(
        ({ from, importDefaultAs }) =>
          `import ${importDefaultAs} from ${JSON.stringify(from)}`,
      )
      .join("\n"),
    data: {
      estree: {
        type: "Program",
        sourceType: "module",
        comments: [],
        body: imports.map(({ from, importDefaultAs }) => ({
          type: "ImportDeclaration",
          specifiers: [
            {
              type: "ImportDefaultSpecifier",
              local: {
                type: "Identifier",
                name: importDefaultAs,
              },
            },
          ],
          source: {
            type: "Literal",
            value: from,
            raw: JSON.stringify(from),
          },
          attributes: [],
        })),
      },
    },
  };
};
