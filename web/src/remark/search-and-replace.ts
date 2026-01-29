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
  const compiled = Object.entries(definitions).map(([key, value]) => ({
    regex: new RegExp(`\\{${key}\\}`, "g"),
    value,
  }));

  return {
    searchAndReplace(text: string): string {
      for (const { regex, value } of compiled) {
        text = text.replace(regex, value);
      }
      return text;
    },
  };
}

export default plugin;
