import type { Heading, Nodes as MdastNodes, Root as MdastRoot } from "mdast";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkParse from "remark-parse";
import remarkStringify from "remark-stringify";
import { unified } from "unified";
import { visit } from "unist-util-visit";

/**
 * For the full llm files we:
 * - Drop the title (the breadcrumb heading replaces it).
 * - Nest the remaining headings one level deeper.
 * - Turn links into full URLs so the file stands on its own.
 */
export function adaptMarkdownForLlmFullFiles(
  baseUrl: string,
  markdown: string,
): string {
  return String(
    getBuiltMarkdownProcessor(baseUrl).processSync(markdown),
  ).trim();
}

const builtMarkdownProcessorByBaseUrl = new Map<
  string,
  ReturnType<typeof createBuiltMarkdownProcessor>
>();

function getBuiltMarkdownProcessor(baseUrl: string) {
  let processor = builtMarkdownProcessorByBaseUrl.get(baseUrl);
  if (!processor) {
    processor = createBuiltMarkdownProcessor(baseUrl);
    builtMarkdownProcessorByBaseUrl.set(baseUrl, processor);
  }
  return processor;
}

function createBuiltMarkdownProcessor(baseUrl: string) {
  return unified()
    .use(remarkParse)
    .use(remarkGfm)
    .use(remarkDirective)
    .use(() => (tree: MdastRoot) => rewriteBuiltDoc(baseUrl, tree))
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

function rewriteBuiltDoc(baseUrl: string, tree: MdastRoot): void {
  dropIndexHeader(tree);
  dropTitleHeading(tree);
  nestHeadingsDeeper(tree);
  makeRootRelativeUrlsAbsolute(baseUrl, tree);
}

/**
 * Each built Markdown file starts with a header pointing at the docs index
 * (a blockquote followed by a thematic break). It is redundant inside the
 * concatenated index, so we drop it.
 */
function dropIndexHeader(tree: MdastRoot): void {
  const [first, second] = tree.children;
  if (first?.type === "blockquote" && second?.type === "thematicBreak") {
    tree.children.splice(0, 2);
  }
}

function dropTitleHeading(tree: MdastRoot): void {
  const titleIndex = tree.children.findIndex(
    (node) => node.type === "heading" && node.depth === 1,
  );
  if (titleIndex !== -1) {
    tree.children.splice(titleIndex, 1);
  }
}

function nestHeadingsDeeper(tree: MdastRoot): void {
  visit(tree, "heading", (heading) => {
    heading.depth = (heading.depth + 1) as Heading["depth"];
  });
}

function makeRootRelativeUrlsAbsolute(baseUrl: string, tree: MdastRoot): void {
  visit(tree, (node: MdastNodes) => {
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
