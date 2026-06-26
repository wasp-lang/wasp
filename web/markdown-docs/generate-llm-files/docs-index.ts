import type {
  DocMetadata,
  LoadedVersion,
} from "@docusaurus/plugin-content-docs";
import { existsSync } from "fs";
import fs from "fs/promises";
import type { Heading, Nodes as MdastNodes, Root as MdastRoot } from "mdast";
import path from "path";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkParse from "remark-parse";
import remarkStringify from "remark-stringify";
import { unified } from "unified";
import { visit } from "unist-util-visit";

import type { LllmDocsContext } from "../context";

const SIDEBAR_CATEGORIES_TO_IGNORE = ["Miscellaneous"];

export interface MarkdownDocsIndex {
  sections: IndexSection[];
}

/**
 * A sidebar, e.g.: `Docs`, `Guides` or `API`.
 */
export interface IndexSection {
  title: string;
  items: IndexItem[];
}

export type IndexItem = IndexCategory | IndexDoc;

/**
 * A sidebar category, e.g.: `Authentication` or `Authentication / Email`.
 */
export interface IndexCategory {
  type: "category";
  label: string;
  items: IndexItem[];
}

/**
 * An idividual item of some sidebar category, e.g.: `Authentication / Email / Overview`.
 */
export interface IndexDoc {
  type: "doc";
  title: string;
  url: string;
  markdown: string;
}

type LoadedSidebarItem = LoadedVersion["sidebars"][string][number];
type DocsById = Map<DocMetadata["id"], DocMetadata>;

/**
 * Builds a markdown docs index for a single docs version, straight from the
 * docs plugin's in-memory content.
 *
 * `Docs` and `Guides` follow their sidebars (categories become titles, docs
 * become links/content). `API` is listed separately: it is large and verbose,
 * so we surface only each package's index page (its overview, which itself
 * links to every symbol).
 */
export async function buildMarkdownDocsIndex(
  context: LllmDocsContext,
  loadedVersion: LoadedVersion,
): Promise<MarkdownDocsIndex> {
  const docsById: DocsById = new Map(
    loadedVersion.docs.map((doc) => [doc.id, doc]),
  );

  const indexSections: IndexSection[] = [
    {
      title: "Docs",
      items: await buildSidebarItems(
        context,
        loadedVersion.sidebars["docs"] ?? [],
        docsById,
      ),
    },
    {
      title: "Guides",
      items: await buildSidebarItems(
        context,
        loadedVersion.sidebars["guides"] ?? [],
        docsById,
      ),
    },
    {
      title: "API",
      items: await buildApiSectionItems(context, loadedVersion.docs),
    },
  ]
    // Some sections can be empty in old Wasp versions. Like `API` and `Guides`.
    .filter((section) => section.items.length > 0);

  return {
    sections: indexSections,
  };
}

async function buildSidebarItems(
  context: LllmDocsContext,
  sidebarItems: LoadedSidebarItem[],
  docsById: DocsById,
): Promise<IndexItem[]> {
  const indexItems: IndexItem[] = [];
  for (const sidebarItem of sidebarItems) {
    const indexItem = await resolveSidebarItem(context, sidebarItem, docsById);
    if (indexItem) {
      indexItems.push(indexItem);
    }
  }
  return indexItems;
}

/**
 * Docusaurus keeps sidebar doc entries as bare `{ type: "doc", id }`, so we
 * join each against the version's docs to recover its permalink and label.
 */
async function resolveSidebarItem(
  context: LllmDocsContext,
  sidebarItem: LoadedSidebarItem,
  docsById: DocsById,
): Promise<IndexItem | null> {
  if (sidebarItem.type === "category") {
    if (SIDEBAR_CATEGORIES_TO_IGNORE.includes(sidebarItem.label)) {
      return null;
    }
    const items = await buildSidebarItems(context, sidebarItem.items, docsById);
    if (items.length === 0) {
      return null;
    }
    return { type: "category", label: sidebarItem.label, items };
  }

  if (sidebarItem.type === "link") {
    if (!sidebarItem.href.startsWith("/")) {
      return null;
    }
    return resolveIndexDoc(
      context,
      stripTrailingSlash(sidebarItem.href),
      sidebarItem.label,
    );
  }

  if (sidebarItem.type === "doc" || sidebarItem.type === "ref") {
    const doc = docsById.get(sidebarItem.id);
    if (!doc) {
      return null;
    }
    const label =
      sidebarItem.label ?? doc.frontMatter.sidebar_label ?? doc.title;
    return resolveIndexDoc(context, stripTrailingSlash(doc.permalink), label);
  }

  // `html` and any future item types have no doc route, so we drop them.
  return null;
}

async function buildApiSectionItems(
  context: LllmDocsContext,
  docs: DocMetadata[],
): Promise<IndexItem[]> {
  const items: IndexItem[] = [];
  for (const doc of docs) {
    const packagePath = extractApiDocsPackageIndexPagePath(doc.id);
    if (packagePath) {
      items.push(
        await resolveIndexDoc(
          context,
          stripTrailingSlash(doc.permalink),
          packagePath,
        ),
      );
    }
  }
  return items;
}

/**
 * Tries to extract the path to the index page of some package's API docs.
 *
 * @example "api/@wasp.sh/spec/index" -> "@wasp.sh/spec"
 */
function extractApiDocsPackageIndexPagePath(docId: string): string | null {
  const API_DOCS_PACKAGE_INDEX_PAGE_PATTERN = /^api\/(.+)\/index$/;
  const match = docId.match(API_DOCS_PACKAGE_INDEX_PAGE_PATTERN);

  if (!match) {
    return null;
  }

  const [_fullMatch, apiDocsPackageIndexPagePath] = match;

  return apiDocsPackageIndexPagePath;
}

const markdownDocumentByRouteCache = new Map<
  string,
  { url: string; markdown: string }
>();

async function resolveIndexDoc(
  context: LllmDocsContext,
  route: string,
  title: string,
): Promise<IndexDoc> {
  let markdownDocument = markdownDocumentByRouteCache.get(route);
  if (!markdownDocument) {
    const markdownFilePath = path.join(context.outDir, route + ".md");
    if (!existsSync(markdownFilePath)) {
      throw Error(`Missing Markdown file for a document: ${markdownFilePath}`);
    }

    const markdown = await fs.readFile(markdownFilePath, "utf8");
    markdownDocument = {
      url: context.baseUrl + route + ".md",
      markdown: processBuiltMarkdown(context.baseUrl, markdown),
    };
    markdownDocumentByRouteCache.set(route, markdownDocument);
  }
  return { type: "doc", title, ...markdownDocument };
}

/**
 * For the concatenated docs we drop the title (the breadcrumb heading
 * replaces it), nest the remaining headings one level deeper, and turn links
 * into full URLs so the file stands on its own.
 */
function processBuiltMarkdown(baseUrl: string, markdown: string): string {
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

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
