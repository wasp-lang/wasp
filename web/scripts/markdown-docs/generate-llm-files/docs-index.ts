import { existsSync } from "fs";
import fs from "fs/promises";
import path from "path";

import { loadPermalinkMaps } from "../permalinks";
import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";
import {
  type ResolvedSidebarItem,
  isSidebarCategory,
  isSidebarLink,
  loadResolvedSidebarsByWaspVersion,
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
 * E.g. `Docs`, `Guides`, `API`.
 */
export interface IndexSection {
  title: string;
  items: IndexItem[];
}

export type IndexItem = IndexCategory | IndexDoc;

/**
 * A sidebar category, e.g. `Authentication` or `Authentication > Email`. Kept
 * nested so the index can mirror the sidebar and each doc keeps its breadcrumb.
 */
export interface IndexCategory {
  type: "category";
  label: string;
  items: IndexItem[];
}

export interface IndexDoc {
  type: "doc";
  title: string;
  docUrl: string;
  processedBody: string;
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

  const sections: IndexSection[] = [];
  for (const { title, sidebarName } of EXPECTED_SIDEBARS) {
    const sidebarItems = sidebars[sidebarName];
    if (!sidebarItems) {
      throw Error(
        `Resolved sidebars are missing an expected sidebar: ${sidebarName}`,
      );
    }
    sections.push({
      title,
      items: await buildSectionItems(sidebarItems, waspVersion),
    });
  }
  sections.push(await buildApiSection("API", waspVersion));

  // Drop empty sections (e.g. older versions without API packages).
  return { sections: sections.filter((section) => section.items.length > 0) };
}

// Mirrors the sidebar tree: links become docs, categories recurse (ignored or
// empty ones are dropped).
async function buildSectionItems(
  sidebarItems: ResolvedSidebarItem[],
  waspVersion: string,
): Promise<IndexItem[]> {
  const items: IndexItem[] = [];
  for (const sidebarItem of sidebarItems) {
    if (isSidebarLink(sidebarItem)) {
      const doc = await resolveSidebarLink(sidebarItem);
      if (doc) {
        items.push(doc);
      }
    } else if (isSidebarCategory(sidebarItem)) {
      if (SIDEBAR_CATEGORIES_TO_IGNORE.includes(sidebarItem.label)) {
        continue;
      }
      const children = await buildSectionItems(sidebarItem.items, waspVersion);
      if (children.length > 0) {
        items.push({
          type: "category",
          label: sidebarItem.label,
          items: children,
        });
      }
    }
  }
  return items;
}

// The API reference is large and verbose, so we list only each package's index
// page (its overview, which itself links to every symbol) rather than every
// function/interface page. Package indexes come from the permalink map.
async function buildApiSection(
  title: string,
  waspVersion: string,
): Promise<IndexSection> {
  const permalinkMap = permalinkMapsByWaspVersion.get(waspVersion) ?? new Map();

  const items: IndexItem[] = [];
  for (const [docId, permalink] of permalinkMap) {
    const match = docId.match(API_PACKAGE_INDEX_REG_EXP);
    if (match) {
      items.push(await resolveDoc(stripTrailingSlash(permalink), match[1]));
    }
  }
  return { title, items };
}

// Keeps only internal links (the ones with a corresponding built `.md`).
async function resolveSidebarLink(link: {
  href: string;
  label: string;
}): Promise<IndexDoc | null> {
  if (!link.href.startsWith("/")) {
    return null;
  }
  return resolveDoc(stripTrailingSlash(link.href), link.label);
}

// Body and URL are derived from the route, so they are cached by it; the same
// page (e.g. a migration guide) is referenced by several versions' sidebars.
const resolvedBodyByRoute = new Map<
  string,
  { docUrl: string; processedBody: string }
>();

async function resolveDoc(route: string, title: string): Promise<IndexDoc> {
  let resolved = resolvedBodyByRoute.get(route);
  if (!resolved) {
    // `trailingSlash` is false, so the page at "/docs/x" is "build/docs/x.html",
    // and its Markdown sibling is "build/docs/x.md".
    const markdownFilePath = path.join(BUILD_DIR, route + ".md");
    if (!existsSync(markdownFilePath)) {
      throw Error(`Missing Markdown file for a document: ${markdownFilePath}`);
    }
    const markdown = await fs.readFile(markdownFilePath, "utf8");
    resolved = {
      docUrl: WASP_BASE_URL + route + ".md",
      processedBody: processBuiltMarkdown(markdown),
    };
    resolvedBodyByRoute.set(route, resolved);
  }
  return { type: "doc", title, ...resolved };
}

// ----------------------------------------------------------------------------
// Markdown helpers
// ----------------------------------------------------------------------------

// The rendered Markdown leads with the page's `# Title` and uses site-absolute
// links. For the concatenated docs we drop the title (the breadcrumb heading
// replaces it), nest the remaining headings one level deeper, and turn links
// into full URLs so the file stands on its own.
function processBuiltMarkdown(markdown: string): string {
  const withoutTitle = markdown.replace(/^#\s+.*\n+/, "");
  const nested = withoutTitle.replace(
    /^(#{1,6})(\s)/gm,
    (_match, hashes, space) =>
      hashes.length < 6 ? `#${hashes}${space}` : `${hashes}${space}`,
  );
  return makeLinksAbsolute(nested).trim();
}

function makeLinksAbsolute(markdown: string): string {
  return markdown.replace(
    /\]\((\/[^)]*)\)/g,
    (_match, sitePath: string) => `](${WASP_BASE_URL}${sitePath})`,
  );
}

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
