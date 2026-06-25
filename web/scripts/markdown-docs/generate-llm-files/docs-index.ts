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

import { BUILD_DIR, WASP_BASE_URL, WEB_PROJECT_ROOT_DIR } from "../constants";
import { loadPermalinkMaps, PermalinkMap } from "./permalinks";
import {
  isSidebarCategory,
  isSidebarLink,
  loadResolvedSidebarsByWaspVersion,
  ResolvedSidebarCategory,
  type ResolvedSidebarItem,
  ResolvedSidebarLink,
} from "./resolved-sidebars";

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

const sidebarsByWaspVersion =
  loadResolvedSidebarsByWaspVersion(WEB_PROJECT_ROOT_DIR);
const permalinkMapsByWaspVersion = loadPermalinkMaps(WEB_PROJECT_ROOT_DIR);

/**
 * Builds a markdown docs index for a specific Wasp version.
 *
 * It crawls the resolved sidebar and permalink map for that Wasp version,
 * and connects the found routes to their generated markdown files.
 */
export async function buildMarkdownDocsIndex(
  waspVersion: string,
): Promise<MarkdownDocsIndex> {
  const sidebars = sidebarsByWaspVersion.get(waspVersion);
  if (!sidebars) {
    throw Error(`Resolved sidebars are missing a Wasp version: ${waspVersion}`);
  }
  const permalinkMap = permalinkMapsByWaspVersion.get(waspVersion);
  if (!permalinkMap) {
    throw Error(`Permalink maps are missing a Wasp version: ${waspVersion}`);
  }

  const indexSections: IndexSection[] = [
    {
      title: "Docs",
      items: await buildSectionItems(sidebars["docs"]),
    },
    {
      title: "Guides",
      items: await buildSectionItems(sidebars["guides"]),
    },
    {
      title: "API",
      items: await buildApiSectionItems(permalinkMap),
    },
  ]
    // Some sections can be empty in old Wasp versions. Like `API` and `Guides`.
    .filter((section) => section.items.length > 0);

  return {
    sections: indexSections,
  };
}

async function buildSectionItems(
  sidebarItems: ResolvedSidebarItem[],
): Promise<IndexItem[]> {
  const indexItems: IndexItem[] = [];
  for (const sidebarItem of sidebarItems) {
    if (isSidebarLink(sidebarItem)) {
      const indexDoc = await resolveSidebarLink(sidebarItem);
      if (indexDoc) {
        indexItems.push(indexDoc);
      }
    } else if (isSidebarCategory(sidebarItem)) {
      if (SIDEBAR_CATEGORIES_TO_IGNORE.includes(sidebarItem.label)) {
        continue;
      }
      const indexCategory = await resolveSidebarCategory(sidebarItem);
      if (indexCategory) {
        indexItems.push(indexCategory);
      }
    }
  }
  return indexItems;
}

async function resolveSidebarLink(
  sidebarLink: ResolvedSidebarLink,
): Promise<IndexDoc | null> {
  if (!sidebarLink.href.startsWith("/")) {
    return null;
  }
  return resolveIndexDoc(
    stripTrailingSlash(sidebarLink.href),
    sidebarLink.label,
  );
}

async function resolveSidebarCategory(
  sidebarCategory: ResolvedSidebarCategory,
): Promise<IndexCategory | null> {
  const sidebarCategoryItems = await buildSectionItems(sidebarCategory.items);

  if (sidebarCategoryItems.length === 0) {
    return null;
  }

  return {
    type: "category",
    label: sidebarCategory.label,
    items: sidebarCategoryItems,
  };
}

/**
 * The API reference is large and verbose, so we list only each package's index
 * page (its overview, which itself links to every symbol).
 */
async function buildApiSectionItems(
  permalinkMap: PermalinkMap,
): Promise<IndexSection["items"]> {
  const items: IndexItem[] = [];
  for (const [docId, permalink] of permalinkMap) {
    const path = extractApiDocsPackageIndexPagePath(docId);
    if (path) {
      items.push(await resolveIndexDoc(stripTrailingSlash(permalink), path));
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
  route: string,
  title: string,
): Promise<IndexDoc> {
  let markdownDocument = markdownDocumentByRouteCache.get(route);
  if (!markdownDocument) {
    const markdownFilePath = path.join(BUILD_DIR, route + ".md");
    if (!existsSync(markdownFilePath)) {
      throw Error(`Missing Markdown file for a document: ${markdownFilePath}`);
    }

    const markdown = await fs.readFile(markdownFilePath, "utf8");
    markdownDocument = {
      url: WASP_BASE_URL + route + ".md",
      markdown: processBuiltMarkdown(markdown),
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
function processBuiltMarkdown(markdown: string): string {
  return String(builtMarkdownProcessor.processSync(markdown)).trim();
}

const builtMarkdownProcessor = unified()
  .use(remarkParse)
  .use(remarkGfm)
  .use(remarkDirective)
  .use(remarkRewriteBuiltDoc)
  .use(remarkStringify, {
    bullet: "-",
    emphasis: "*",
    strong: "*",
    fence: "`",
    fences: true,
    rule: "-",
    listItemIndent: "one",
  });

function remarkRewriteBuiltDoc(): (tree: MdastRoot) => void {
  return (tree: MdastRoot): void => {
    dropIndexHeader(tree);
    dropTitleHeading(tree);
    nestHeadingsDeeper(tree);
    makeRootRelativeUrlsAbsolute(tree);
  };
}

/**
 * Each built Markdown file starts with a header pointing at the docs index.
 * It is redundant inside the concatenated index, so we drop it.
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

function makeRootRelativeUrlsAbsolute(tree: MdastRoot): void {
  visit(tree, (node: MdastNodes) => {
    const hasRootRelativeUrl =
      (node.type === "link" ||
        node.type === "image" ||
        node.type === "definition") &&
      node.url.startsWith("/");
    if (hasRootRelativeUrl) {
      node.url = WASP_BASE_URL + node.url;
    }
  });
}

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
