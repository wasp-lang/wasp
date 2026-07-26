import GithubSlugger from "github-slugger";
import type * as hast from "hast";
import { visit } from "unist-util-visit";
import { hastTextContent } from "./hast-helpers";

const HEADING_TAG_NAMES = ["h1", "h2", "h3", "h4", "h5", "h6"];

/**
 * Docusaurus allows custom heading anchors (`## Title {#custom-id}`),
 * which are lost when converting the rendered HTML back to markdown,
 * breaking `#custom-id` links from other markdown docs.
 *
 * Re-appends the `{#id}` suffix to a heading whenever its rendered id
 * differs from the auto-generated slug of its text. Uses the same
 * slugger as Docusaurus, so default anchors stay unannotated.
 *
 * Must run after `rehypeReduceDocusaurusPageToValidMarkdownContent`,
 * so heading text is clean of hash-links when compared against the
 * slugger output.
 */
export function rehypeAnnotateCustomHeadingAnchors(): (
  root: hast.Root,
) => void {
  return (root) => {
    const slugger = new GithubSlugger();

    visit(root, "element", (node) => {
      if (!HEADING_TAG_NAMES.includes(node.tagName)) {
        return;
      }
      const headingId = node.properties?.id;
      if (typeof headingId !== "string" || !headingId) {
        return;
      }

      const defaultHeadingId = slugger.slug(hastTextContent(node).trim());
      if (headingId !== defaultHeadingId) {
        node.children.push({ type: "text", value: ` {#${headingId}}` });
      }
    });
  };
}
