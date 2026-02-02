import type { Root } from "mdast";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import docsVersions from "../../versions.json";

const latestWaspVersion = docsVersions[0];

const replacer = createReplacer({
  latestWaspVersion: `^${latestWaspVersion}`,
  pinnedLatestWaspVersion: latestWaspVersion,
  // NOTE: Don't change Wasp's lowest supported Node version without updating it
  // in all required places. Check /.nvmrc for the full list.
  minimumNodeJsVersion: "22.12",
});

const plugin: Plugin<[], Root> = () => (tree) => {
  visit(tree, (node) => {
    if (node.type === "code" || node.type === "text") {
      node.value = replacer.searchAndReplace(node.value);
    }
  });
};

/**
  Accepts a record of key-value pairs. Replaces `{key}` with `value` when
  the text is searched and replaced.
*/
function createReplacer(definitions: Record<string, string>) {
  return {
    searchAndReplace(text: string): string {
      for (const [key, value] of Object.entries(definitions)) {
        text = text.replaceAll(`{${key}}`, value);
      }
      return text;
    },
  };
}

export default plugin;
