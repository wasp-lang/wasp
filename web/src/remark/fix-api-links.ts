import type * as md from "mdast";
import * as path from "node:path";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";

const API_FOLDER = "docs/api/";
const URL_PREFIX = "https://wasp.sh";

/*
  This plugin turns absolute URLs to the Wasp website itself (inside the API
  docs) into relative links, so that they work both on the website and in the
  generated markdown files; and can be checked for broken links.
*/
const fixAPILinks: Plugin<[], md.Root> = () => (tree, file) => {
  const relativePath = path.relative(file.cwd, file.path);
  if (!relativePath.startsWith(API_FOLDER)) return;

  visit(tree, "link", (node) => {
    if (node.url.startsWith(URL_PREFIX)) {
      node.url = node.url.slice(URL_PREFIX.length);
    }
  });
};

export default fixAPILinks;
