// Copied from
// https://github.com/redwoodjs/redwood/blob/bd903c5755925ea7174775a2fdaba371b700c910/docs/src/remark/auto-import-tabs.js
// and modified to work with nested tab usage.

import type { Root } from "mdast";
import type { Plugin } from "unified";
import { EXIT, visit } from "unist-util-visit";
import { makeImports } from "./util/make-imports";

const plugin: Plugin<[], Root> = () => (tree, _file) => {
  let needsImports = false;

  visit(tree, "mdxJsxFlowElement", (node) => {
    if (node.name === "Tabs" || node.name === "TabItem" || node.name === "BeforeAfter") {
      needsImports = true;
      return EXIT;
    }
  });

  if (needsImports) {
    // Add `import` nodes to the top of the parsed file
    tree.children.unshift(
      makeImports([
        { from: "@theme/Tabs", importDefaultAs: "Tabs" },
        { from: "@theme/TabItem", importDefaultAs: "TabItem" },
        { from: "../components/BeforeAfter", importDefaultAs: "BeforeAfter" },
      ]),
    );
  }
};

export default plugin;
