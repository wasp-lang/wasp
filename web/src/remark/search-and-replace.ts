import type { Root } from "mdast";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import docsVersions from "../../versions.json";

const latestWaspVersion = docsVersions[0];

const replacements = [
  {
    search: /{latestWaspVersion}/g,
    replace: `^${latestWaspVersion}`,
  },
];

export function visitor(tree: Root) {
  visit(tree, (node) => {
    // NOTE: For now we only replace in code blocks to keep
    // the search and replace logic simple.
    if (node.type === "code") {
      for (const { search, replace } of replacements) {
        node.value = node.value.replace(search, replace);
      }
    }
  });
}

const plugin: Plugin<[], Root> = () => visitor;

export default plugin;
