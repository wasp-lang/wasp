import type {
  DocMetadata,
  LoadedVersion,
} from "@docusaurus/plugin-content-docs";
import { existsSync } from "fs";
import fs from "fs/promises";
import path from "path";

import type { LlmFilesContext } from "./context";
import { stripTrailingSlash } from "../helpers";
import { adaptMarkdownForLlmFullFiles } from "./adapt-markdown";

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
 * Represents a docs page we can visit. Has markdown contnet.
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
  context: LlmFilesContext,
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
        // TODO: Should this be part of llm files?
        return null;
      }
      return resolveIndexDoc(
        context,
        stripTrailingSlash(sidebarItem.href),
        sidebarItem.label,
      );

    case "doc":
    case "ref":
      const doc = docsById.get(sidebarItem.id);
      if (!doc) {
        // TODO: when does this happen?
        return null;
      }
      const title =
        sidebarItem.label ?? doc.frontMatter.sidebar_label ?? doc.title;
      return resolveIndexDoc(context, stripTrailingSlash(doc.permalink), title);

    case "html":
      return null;

    default:
      assertUnreachable(
        sidebarItem,
        `Unhandled Docusaurus sidebar item: ${JSON.stringify(sidebarItem, undefined, 2)}`,
      );
  }
}

async function buildApiSectionItems(
  context: LlmFilesContext,
  docs: DocMetadata[],
): Promise<IndexItem[]> {
  const items: IndexItem[] = [];
  for (const doc of docs) {
    const packageName = extractApiDocsPackageName(doc.id);
    if (packageName) {
      items.push(
        await resolveIndexDoc(
          context,
          stripTrailingSlash(doc.permalink),
          packageName,
        ),
      );
    }
  }
  return items;
}

/**
 * Tries to extract the name of some package's API docs.
 * Does it by trying to match API package index package.
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
  let markdownDocument = markdownDocumentByRouteCache.get(route);
  if (!markdownDocument) {
    const markdownFilePath = path.join(context.outDir, route + ".md");
    if (!existsSync(markdownFilePath)) {
      throw Error(`Missing Markdown file for a document: ${markdownFilePath}`);
    }

    const markdown = await fs.readFile(markdownFilePath, "utf8");
    markdownDocument = {
      url: context.baseUrl + route + ".md",
      markdown: adaptMarkdownForLlmFullFiles(context.baseUrl, markdown),
    };
    markdownDocumentByRouteCache.set(route, markdownDocument);
  }
  return { type: "doc", title, ...markdownDocument };
}

function assertUnreachable(_x: never, message: string): never {
  throw new Error(message);
}
