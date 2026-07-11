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
export function adaptMarkdownForLlmsFullFiles(
  baseUrl: string,
  markdown: string,
): string {
  return String(getLlmsFullMarkdownProcessor(baseUrl).processSync(markdown));
}

const llmsFullMarkdownProcessorByBaseUrl = new Map<
  string,
  ReturnType<typeof createLlmsFullMarkdownProcessor>
>();

function getLlmsFullMarkdownProcessor(baseUrl: string) {
  let processor = llmsFullMarkdownProcessorByBaseUrl.get(baseUrl);
  if (!processor) {
    processor = createLlmsFullMarkdownProcessor(baseUrl);
    llmsFullMarkdownProcessorByBaseUrl.set(baseUrl, processor);
  }
  return processor;
}

function createLlmsFullMarkdownProcessor(baseUrl: string) {
  return unified()
    .use(remarkParse)
    .use(remarkGfm)
    .use(remarkDirective)
    .use(remarkAdaptMarkdownForLlmsFullFiles, baseUrl)
    .use(remarkStringify, {
      bullet: "-",
      emphasis: "*",
      strong: "*",
      fence: "`",
      fences: true,
      rule: "-",
      listItemIndent: "one",
    });
}

function remarkAdaptMarkdownForLlmsFullFiles(
  baseUrl: string,
): (tree: mdast.Root) => void {
  return (tree: mdast.Root) => {
    dropDocumentHeading(tree);
    nestHeadingsDeeper(tree);
    makeRootRelativeUrlsAbsolute(baseUrl, tree);
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

function makeRootRelativeUrlsAbsolute(baseUrl: string, tree: mdast.Root): void {
  visit(tree, (node: mdast.Nodes) => {
    const hasRootRelativeUrl =
      (node.type === "link" ||
        node.type === "image" ||
        node.type === "definition") &&
      node.url.startsWith("/");
    if (hasRootRelativeUrl) {
      node.url = baseUrl + node.url;
    }
  });
}
