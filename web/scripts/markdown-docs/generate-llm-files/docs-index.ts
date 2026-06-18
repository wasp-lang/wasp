import { existsSync } from "fs";
import fs from "fs/promises";
import path from "path";

import { loadPermalinkMaps, PermalinkMap } from "../permalinks";
import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";
import {
  isSidebarCategory,
  isSidebarLink,
  loadResolvedSidebarsByWaspVersion,
  ResolvedSidebarCategory,
  type ResolvedSidebarItem,
  ResolvedSidebarLink,
} from "./resolved-sidebars";

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

const EXPECTED_SIDEBARS: Array<{ title: string; sidebarName: string }> = [
  { title: "Docs", sidebarName: "docs" },
  { title: "Guides", sidebarName: "guides" },
];
const SIDEBAR_CATEGORIES_TO_IGNORE = ["Miscellaneous"];
// A typedoc package's index page, e.g. "api/@wasp.sh/spec/index". We list these
// (the package overview) in the API section instead of every symbol page.
const API_PACKAGE_INDEX_REG_EXP = /^api\/(.+)\/index$/;

export interface DocsIndex {
  sections: IndexSection[];
}

/**
 * Esentially, a sidebar of `Docs`, `Guides` or `API`.
 */
export interface IndexSection {
  title: string;
  items: IndexItem[];
}

export type IndexItem = IndexCategory | IndexDoc;

/**
 * A sidebar category, e.g. `Authentication` or `Authentication / Email`.
 */
export interface IndexCategory {
  type: "category";
  label: string;
  items: IndexItem[];
}

/**
 * An idividual item of some sidebar category, e.g. `Authentication / Email / Overview`.
 */
export interface IndexDoc {
  type: "doc";
  title: string;
  url: string;
  markdown: string;
}

const sidebarsByWaspVersion = loadResolvedSidebarsByWaspVersion(SITE_ROOT);
const permalinkMapsByWaspVersion = loadPermalinkMaps(SITE_ROOT);

/**
 * Builds the nested index of a version's docs, guides, and API packages from the
 * resolved sidebars and the generated Markdown docs. Each doc's title is the
 * sidebar label; its category breadcrumb comes from the surrounding categories.
 */
export async function buildDocsIndex(waspVersion: string): Promise<DocsIndex> {
  const sidebars = sidebarsByWaspVersion.get(waspVersion);
  if (!sidebars) {
    throw Error(`Resolved sidebars are missing a Wasp version: ${waspVersion}`);
  }
  const permalinkMap = permalinkMapsByWaspVersion.get(waspVersion);
  if (!permalinkMap) {
    throw Error(`Permalink maps are missing a Wasp version: ${waspVersion}`);
  }

  const indexSections: IndexSection[] = [];
  for (const { title, sidebarName } of EXPECTED_SIDEBARS) {
    const sidebarItems = sidebars[sidebarName];
    if (!sidebarItems) {
      throw Error(
        `Resolved sidebars are missing an expected sidebar: ${sidebarName}`,
      );
    }
    indexSections.push({
      title,
      items: await buildSectionItems(sidebarItems),
    });
  }
  indexSections.push({
    title: "API",
    items: await buildApiSectionItems(permalinkMap),
  });

  const validIndexSections = indexSections.filter(
    (section) => section.items.length > 0,
  );

  return {
    sections: validIndexSections,
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
    const match = docId.match(API_PACKAGE_INDEX_REG_EXP);
    if (match) {
      items.push(
        await resolveIndexDoc(stripTrailingSlash(permalink), match[1]),
      );
    }
  }
  return items;
}

const markdownDocumentByRoute = new Map<
  string,
  { url: string; markdown: string }
>();

async function resolveIndexDoc(
  route: string,
  title: string,
): Promise<IndexDoc> {
  let markdownDocument = markdownDocumentByRoute.get(route);
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
    markdownDocumentByRoute.set(route, markdownDocument);
  }
  return { type: "doc", title, ...markdownDocument };
}

/**
 * For the concatenated docs we drop the title (the breadcrumb heading
 * replaces it), nest the remaining headings one level deeper, and turn links
 * into full URLs so the file stands on its own.
 */
function processBuiltMarkdown(markdown: string): string {
  const withoutTitle = markdown.replace(/^#\s+.*\n+/, "");
  const withNestedSubTitles = withoutTitle.replace(
    /^(#+)(\s)/gm,
    (match) => `#${match}`,
  );
  return makePermalinksAbsolute(withNestedSubTitles).trim();
}

function makePermalinksAbsolute(markdown: string): string {
  return markdown.replace(
    /\]\((\/[^)]*)\)/g,
    (_match, sitePath: string) => `](${WASP_BASE_URL}${sitePath})`,
  );
}

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
