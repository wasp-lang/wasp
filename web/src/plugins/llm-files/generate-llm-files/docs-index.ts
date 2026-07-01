import type {
  DocMetadata,
  LoadedVersion,
} from "@docusaurus/plugin-content-docs";
import { existsSync } from "fs";
import fs from "fs/promises";
import path from "path";

import { stripTrailingSlash } from "../helpers";
import { adaptMarkdownForLlmsFullFiles } from "./adapt-markdown";
import type { LlmFilesContext } from "./context";

const SIDEBAR_CATEGORIES_TO_IGNORE = ["Miscellaneous"];

/**
 * An index of all markdown docs that we want to be part
 * of our `llms*.txt` files.
 *
 * The index follows the docs categories hirarchical structure.
 */
export interface LlmFilesMarkdownDocsIndex {
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
 * A sidebar category, e.g.: "Authentication" or "Authentication / Email".
 * Server organizational purpose, has no actual content.
 */
export interface IndexCategory {
  type: "category";
  title: string;
  items: IndexItem[];
}

/**
 * An idividual item of some sidebar category, e.g.: "Authentication / Email / Overview".
 * Represents a docs page we can visit.
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
 * Builds a {@link LlmFilesMarkdownDocsIndex} for a single Wasp version.
 *
 * Converts sidebars (`Docs`, `Guides`, `API`...) into index sections,
 * where categories become titles, and docs become content.
 *
 * `API` index section is handled specially.
 * We only include API package's index pages into the index.
 * Otherwise the output would be too verbose.
 */
export async function buildLlmFilesMarkdownDocsIndex(
  context: LlmFilesContext,
  loadedVersion: LoadedVersion,
): Promise<LlmFilesMarkdownDocsIndex> {
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
  context: LlmFilesContext,
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
 * join each against the version's docs to recover its permalink and title.
 */
async function resolveSidebarItem(
  context: LlmFilesContext,
  sidebarItem: LoadedSidebarItem,
  docsById: DocsById,
): Promise<IndexItem | null> {
  switch (sidebarItem.type) {
    case "category":
      if (SIDEBAR_CATEGORIES_TO_IGNORE.includes(sidebarItem.label)) {
        return null;
      }
      const items = await buildSidebarItems(
        context,
        sidebarItem.items,
        docsById,
      );
      if (items.length === 0) {
        // A category without any items, shouldn't really happen.
        return null;
      }
      return { type: "category", title: sidebarItem.label, items };

    case "link":
      if (!sidebarItem.href.startsWith("/")) {
        // External link in sidebar. E.g. example apps link and roadmap link.
        // TODO: Should this be part of the llm files?
        return null;
      }
      return resolveIndexDoc(context, sidebarItem.href, sidebarItem.label);

    case "doc":
    case "ref":
      const doc = docsById.get(sidebarItem.id);
      if (!doc) {
        // This should be unreachable.
        throw new Error(`Sidebar references unknown doc id: ${sidebarItem.id}`);
      }
      const title =
        sidebarItem.label ?? doc.frontMatter.sidebar_label ?? doc.title;
      return resolveIndexDoc(context, doc.permalink, title);

    case "html":
      return null;

    default:
      assertUnreachable(
        sidebarItem,
        `Unhandled Docusaurus sidebar item: ${JSON.stringify(sidebarItem, undefined, 2)}`,
      );
  }
}

/**
 * For each unique package in API docs, resolve its index page.
 */
async function buildApiSectionItems(
  context: LlmFilesContext,
  docs: DocMetadata[],
): Promise<IndexItem[]> {
  const items: IndexItem[] = [];
  for (const doc of docs) {
    const packageName = extractApiDocsPackageName(doc.id);
    if (packageName) {
      items.push(await resolveIndexDoc(context, doc.permalink, packageName));
    }
  }
  return items;
}

/**
 * Tries to extract the name of some package in API docs.
 * Does it by trying to match a document id to the API docs package index page.
 *
 * Depends on `typedoc` package generated output format.
 *
 * @example "api/@wasp.sh/spec/index" -> "@wasp.sh/spec"
 */
function extractApiDocsPackageName(docId: string): string | null {
  const API_DOCS_PACKAGE_INDEX_PAGE_PATTERN = /^api\/(.+)\/index$/;
  const match = docId.match(API_DOCS_PACKAGE_INDEX_PAGE_PATTERN);

  if (!match) {
    return null;
  }

  const [_fullMatch, apiDocsPackageName] = match;

  return apiDocsPackageName;
}

const markdownDocumentByRouteCache = new Map<
  string,
  { url: string; markdown: string }
>();

async function resolveIndexDoc(
  context: LlmFilesContext,
  route: string,
  title: string,
): Promise<IndexDoc> {
  const normalizedRoute = stripTrailingSlash(route);
  const markdownRoute = normalizedRoute + ".md";

  let markdownDocument = markdownDocumentByRouteCache.get(normalizedRoute);
  if (!markdownDocument) {
    const markdownFilePath = path.join(context.outDir, markdownRoute);
    if (!existsSync(markdownFilePath)) {
      throw Error(
        `Missing a markdown file for a document: "${markdownFilePath}"`,
      );
    }

    const markdown = await fs.readFile(markdownFilePath, "utf8");
    markdownDocument = {
      url: context.baseUrl + markdownRoute,
      markdown: adaptMarkdownForLlmsFullFiles(context.baseUrl, markdown),
    };
    markdownDocumentByRouteCache.set(route, markdownDocument);
  }
  return { type: "doc", title, ...markdownDocument };
}

function assertUnreachable(_x: never, message: string): never {
  throw new Error(message);
}
