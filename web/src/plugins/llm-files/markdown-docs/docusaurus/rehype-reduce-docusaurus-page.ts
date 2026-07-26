import type * as hast from "hast";
import * as hastSelect from "hast-util-select";
import { SKIP, visit } from "unist-util-visit";
import { hasClass } from "./hast-helpers";

/**
 * These selectors mark the HTML containers whose content originated from
 * markdown (MDX) sources. Docusaurus wraps that content in a different
 * container depending on the page type.
 *
 * If none of them match a page, we fail the build.
 */
const MARKDOWN_CONTENT_CONTAINER_SELECTORS = [
  // Used by docs. Set by Docusaurus.
  ".theme-doc-markdown",
  // Used by posts. Set by us in the ejected `BlogLayout` theme component.
  "article",
];

/**
 * Reduces the parsed Docusaurus page to just its markdown content container.
 * Drops unnecessary nodes that would otherwise leak into the markdown, like
 * comments.
 *
 * @param skipElementClass
 * Elements with this class will be skipped, removing them from the
 * markdown AST.
 */
export function rehypeReduceDocusaurusPageToValidMarkdownContent(
  skipElementClass: string,
): (root: hast.Root) => void {
  return (root) => {
    root.children = [findMarkdownContentContainer(root)];

    visit(root, (node, index, parent) => {
      if (
        parent !== undefined &&
        index !== undefined &&
        isSkippableNode(node, skipElementClass)
      ) {
        parent.children.splice(index, 1);
        return [SKIP, index];
      }
    });
  };
}

/**
 * @throws {Error} If none of the {@link MARKDOWN_CONTENT_CONTAINER_SELECTORS} match.
 */
function findMarkdownContentContainer(root: hast.Root): hast.Element {
  for (const selector of MARKDOWN_CONTENT_CONTAINER_SELECTORS) {
    const markdownContentContainer = hastSelect.select(selector, root);
    if (markdownContentContainer) {
      return markdownContentContainer;
    }
  }
  throw Error(
    "Unable to find content containers for markdown conversion. Maybe the Docusaurus DOM theme changed?",
  );
}

function isSkippableNode(node: hast.Nodes, skipElementClass: string): boolean {
  // React injects empty `<!-- -->` comments around dynamic values.
  if (node.type === "comment") {
    return true;
  }

  if (node.type === "element") {
    if (isDocusaurusHeadingHashLink(node)) {
      return true;
    }

    const hasSkipInMarkdownDocsClass = hasClass(node, skipElementClass);
    if (hasSkipInMarkdownDocsClass) {
      return true;
    }
  }

  return false;
}

/**
 * A hash link is a link that appears next to headings as # symbol on hover.
 * Clicking it changes the URL to include the title as URI fragment.
 *
 * In markdown it creates unnecessary noise.
 *
 * @example
 * Generated markdown without this function:
 * ```md
 * ## When to use Wasp[#](https://wasp.sh/docs#when-to-use-wasp).
 * ```
 * Generated markdown with this function:
 * ```md
 * ## When to use Wasp
 * ```
 */
function isDocusaurusHeadingHashLink(node: hast.Element) {
  return node.tagName === "a" && hasClass(node, "hash-link");
}
