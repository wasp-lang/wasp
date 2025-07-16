// Copied from
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/remark/file-ext-switcher.js

import type { Nodes, Root } from "mdast";
import type {} from "mdast-util-mdx"; // empty type-only import to register mdx types into mdast
import type { Plugin } from "unified";
import { SKIP, visit } from "unist-util-visit";
import { makeImports } from "./util/make-imports";

const plugin: Plugin<[], Root> = () => {
  let needsImport = false;

  return (tree, _file) => {
    visit(tree, "inlineCode", (node) => {
      if (/\w\.\{jsx?,tsx?}$/.test(node.value)) {
        needsImport = true;

        const newNode: Nodes = {
          type: "mdxJsxTextElement",
          name: "FileExtSwitcher",
          attributes: [
            {
              type: "mdxJsxAttribute",
              name: "path",
              value: node.value,
            },
          ],
          children: [],
        };

        Object.assign(node, newNode);

        return SKIP;
      }
    });

    if (needsImport) {
      tree.children.unshift(
        makeImports([
          {
            from: "@site/src/components/FileExtSwitcher",
            importDefaultAs: "FileExtSwitcher",
          },
        ]),
      );
    }
  };
};

export default plugin;
