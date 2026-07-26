import type * as mdast from "mdast";
import { visit } from "unist-util-visit";

/**
 * Rewrites root-relative URLs (`/docs/...`) into absolute ones
 * (`https://wasp.sh/docs/...`).
 */
export function remarkAbsolutizeUrls(
  baseUrl: string,
): (root: mdast.Root) => void {
  return (root) => {
    visit(root, (node) => {
      if (
        node.type === "link" ||
        node.type === "image" ||
        node.type === "definition"
      ) {
        if (isRootRelativeUrl(node.url)) {
          node.url = baseUrl + node.url;
        }
      }
    });
  };
}

function isRootRelativeUrl(url: string): boolean {
  // `//host/path` URLs are protocol-relative, not root-relative.
  return url.startsWith("/") && !url.startsWith("//");
}
