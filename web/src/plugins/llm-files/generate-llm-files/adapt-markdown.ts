import type * as mdast from "mdast";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkParse from "remark-parse";
import remarkStringify from "remark-stringify";
import { unified } from "unified";
import { visit } from "unist-util-visit";

/**
 * Adapts the generated markdown docs so that they make sense
 * in context of `llms-full*.txt` files.
 *
 * @see {@link remarkAdaptMarkdownForLlmsFullFiles} for more details.
 */
export function adaptMarkdownForLlmsFullFiles(markdown: string): string {
  return String(llmsFullMarkdownProcessor.processSync(markdown));
}

const llmsFullMarkdownProcessor = unified()
  .use(remarkParse)
  .use(remarkGfm)
  .use(remarkDirective)
  .use(remarkAdaptMarkdownForLlmsFullFiles)
  .use(remarkStringify, {
    bullet: "-",
    emphasis: "*",
    strong: "*",
    fence: "`",
    fences: true,
    rule: "-",
    listItemIndent: "one",
  });

function remarkAdaptMarkdownForLlmsFullFiles(): (tree: mdast.Root) => void {
  return (tree: mdast.Root) => {
    dropDocumentHeading(tree);
    nestHeadingsDeeper(tree);
  };
}

/**
 * `llms-full*.txt` files generate breadcrumb headings:
 * e.g. "Authentication / Auth Hooks" instead of "Auth Hooks".
 *
 * To avoid duplicate headings we drop the pre-existing heading.
 */
function dropDocumentHeading(tree: mdast.Root): void {
  const headingIndex = tree.children.findIndex(
    (node) => node.type === "heading" && node.depth === 1,
  );
  if (headingIndex !== -1) {
    tree.children.splice(headingIndex, 1);
  }
}

/**
 * All category headings are nested under a "section title",
 * e.g. "Docs", "Guides", "API".
 *
 * So we have to make them 1 depth deeper.
 */
function nestHeadingsDeeper(tree: mdast.Root): void {
  visit(tree, "heading", (heading) => {
    heading.depth = Math.min(heading.depth + 1, 6) as mdast.Heading["depth"];
  });
}
