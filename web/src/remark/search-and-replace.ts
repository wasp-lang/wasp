import type { Root } from "mdast";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import docsVersions from "../../versions.json";

const latestWaspVersion = docsVersions[0];

const plugin: Plugin<[], Root> = () => (tree, file) => {
  // In versioned docs we want the placeholders to refer to that doc's version,
  // not to the latest one. For "current" (unversioned) docs we fall back to the
  // latest version.
  const waspVersion = getWaspVersionFromFilePath(file.path) ?? latestWaspVersion;

  const replacer = createReplacer({
    latestWaspVersion: `^${waspVersion}`,
    pinnedLatestWaspVersion: waspVersion,
    // NOTE: Don't change Wasp's lowest supported Node version without updating it
    // in all required places. Check /.nvmrc for the full list.
    minimumNodeJsVersion: "24.14.1",
  });

  visit(tree, (node) => {
    if (node.type === "code" || node.type === "text") {
      node.value = replacer.searchAndReplace(node.value);
    }
  });
};

/**
  Extracts the Wasp version from a versioned docs file path
  (e.g. `.../versioned_docs/version-0.20/...` → `0.20`).
  Returns `undefined` for "current" (unversioned) docs.
*/
function getWaspVersionFromFilePath(filePath: string): string | undefined {
  const match = filePath.match(/versioned_docs\/version-([^/]+)\//);
  return match?.[1];
}

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
