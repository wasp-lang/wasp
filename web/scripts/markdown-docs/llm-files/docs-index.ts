import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import { existsSync } from "fs";
import { loadPermalinkMaps, normalizePathToDocId } from "../permalinks";
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
const VERSIONED_DOCS_DIR = path.join(SITE_ROOT, "versioned_docs");

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
  groups: IndexGroup[];
}

/**
 * E.g. `Getting Started`, `Tutorial`, `Data Model`, `Deployment`...
 */
export interface IndexGroup {
  label: string;
  docs: DocEntry[];
}

export interface DocEntry {
  title: string;
  docUrl: string;
  processedBody: string;
}

// A doc to resolve, identified by its served route. `docId` is optional and only
// used to read the `title-llm` frontmatter override.
interface DocRef {
  route: string;
  label: string;
  docId?: string;
}

const sidebarsByWaspVersion = loadResolvedSidebarsByWaspVersion(SITE_ROOT);
const permalinkMapsByWaspVersion = loadPermalinkMaps(SITE_ROOT);

/**
 * Builds the categorized index of a version's docs, guides, and API packages
 * from the resolved sidebars and the generated Markdown docs. Empty sections
 * (e.g. older versions without guides or API) are dropped.
 */
export async function buildDocsIndex(waspVersion: string): Promise<DocsIndex> {
  const sidebars = sidebarsByWaspVersion.get(waspVersion);
  if (!sidebars) {
    throw Error(`Resolved sidebars are missing a Wasp verison: ${waspVersion}`);
  }

  const sections: IndexSection[] = [];
  for (const { title, sidebarName } of EXPECTED_SIDEBARS) {
    if (!sidebars[sidebarName]) {
      throw Error(
        `Resolved sidebars are missing an expected sidebar. Expected: ${sidebarName}`,
      );
    }
    sections.push(
      await buildSidebarSection(title, sidebars[sidebarName], waspVersion),
    );
  }
  sections.push(await buildApiSection("API", waspVersion));

  return { sections };
}

async function buildSidebarSection(
  title: string,
  sidebarItems: ResolvedSidebarItem[],
  waspVersion: string,
): Promise<IndexSection> {
  const rawGroups = extractSectionGroups(sidebarItems);

  const groups: IndexGroup[] = [];
  for (const rawGroup of rawGroups) {
    const docs = await resolveDocRefs(rawGroup.docRefs, waspVersion);
    groups.push({ label: rawGroup.label, docs });
  }
  return { title, groups };
}

// The API reference is large and verbose, so we list only each package's index
// page (its overview, which itself links to every symbol) rather than every
// function/interface page. Package indexes come from the permalink map.
async function buildApiSection(
  title: string,
  waspVersion: string,
): Promise<IndexSection> {
  const permalinkMap = permalinkMapsByWaspVersion.get(waspVersion) ?? new Map();
  if (!permalinkMap) {
    throw Error(`Permalink maps are missing a Wasp verison: ${waspVersion}`);
  }

  const packageRefs: DocRef[] = [];
  for (const [docId, permalink] of permalinkMap) {
    const match = docId.match(API_PACKAGE_INDEX_REG_EXP);
    if (match) {
      packageRefs.push({
        route: stripTrailingSlash(permalink),
        label: match[1],
        docId,
      });
    }
  }

  const docs = await resolveDocRefs(packageRefs, waspVersion);
  return { title, groups: docs.length > 0 ? [{ label: "", docs }] : [] };
}

interface RawGroup {
  label: string;
  docRefs: DocRef[];
}

// Turns a sidebar's top-level items into groups: each top-level category becomes
// a group (its nested sub-categories flattened in), and any loose top-level
// links are collected into a single label-less group rendered first.
function extractSectionGroups(sidebarItems: ResolvedSidebarItem[]): RawGroup[] {
  const groups: RawGroup[] = [];
  const looseDocRefs: DocRef[] = [];

  for (const item of sidebarItems) {
    if (isSidebarCategory(item)) {
      if (SIDEBAR_CATEGORIES_TO_IGNORE.includes(item.label)) {
        continue;
      }
      const docRefs = collectDocRefs(item.items);
      groups.push({ label: item.label, docRefs });
    } else if (isSidebarLink(item)) {
      const ref = sidebarLinkToDocRef(item);
      if (ref) {
        looseDocRefs.push(ref);
      }
    }
  }

  // TODO:?
  if (looseDocRefs.length > 0) {
    groups.unshift({ label: "", docRefs: looseDocRefs });
  }
  return groups;
}

function collectDocRefs(sidebarItems: ResolvedSidebarItem[]): DocRef[] {
  const docRefs: DocRef[] = [];
  for (const item of sidebarItems) {
    if (isSidebarLink(item)) {
      const ref = sidebarLinkToDocRef(item);
      if (ref) {
        docRefs.push(ref);
      }
    } else if (isSidebarCategory(item)) {
      docRefs.push(...collectDocRefs(item.items));
    }
  }
  return docRefs;
}

// Keeps only internal links (the ones with a corresponding built `.md`).
function sidebarLinkToDocRef(link: {
  href: string;
  label: string;
  docId?: string;
}): DocRef | null {
  if (!link.href.startsWith("/")) {
    return null;
  }
  return {
    route: stripTrailingSlash(link.href),
    label: link.label,
    docId: link.docId,
  };
}

async function resolveDocRefs(
  docRefs: DocRef[],
  waspVersion: string,
): Promise<DocEntry[]> {
  const entries: DocEntry[] = [];
  for (const ref of docRefs) {
    const entry = await resolveDocRef(ref, waspVersion);
    if (entry) {
      entries.push(entry);
    }
  }
  return entries;
}

// Cached by built Markdown path: the same page (e.g. a migration guide) is
// referenced by several versions' sidebars, so we only read it once.
const docEntryCache = new Map<string, DocEntry | null>();

async function resolveDocRef(
  docRef: DocRef,
  waspVersion: string,
): Promise<DocEntry | null> {
  // `trailingSlash` is false, so the page at "/docs/x" is "build/docs/x.html",
  // and its Markdown sibling is "build/docs/x.md".
  const docMarkdownFilePath = path.join(BUILD_DIR, docRef.route + ".md");
  if (docEntryCache.has(docMarkdownFilePath)) {
    return docEntryCache.get(docMarkdownFilePath)!;
  }

  if (!existsSync(docMarkdownFilePath)) {
    throw Error(`Missing markdown file for a document: ${docMarkdownFilePath}`);
  }
  const markdown = await fs.readFile(docMarkdownFilePath, "utf8");

  const entry: DocEntry = {
    title: await readDocTitle(docRef, waspVersion),
    docUrl: WASP_BASE_URL + docRef.route + ".md",
    processedBody: processBuiltMarkdown(markdown),
  };
  docEntryCache.set(docMarkdownFilePath, entry);
  return entry;
}

// We read the title from the source frontmatter so the `title-llm` override
// keeps working; we fall back to the sidebar label otherwise.
async function readDocTitle(
  docRef: DocRef,
  waspVersion: string,
): Promise<string> {
  if (!docRef.docId) {
    return docRef.label;
  }
  const relativePath = getDocIdToPathMap(waspVersion).get(docRef.docId);
  if (!relativePath) {
    return docRef.label;
  }
  const sourcePath = path.join(
    VERSIONED_DOCS_DIR,
    `version-${waspVersion}`,
    relativePath,
  );
  const raw = await readFileOrNull(sourcePath);
  if (raw === null) {
    return docRef.label;
  }
  const { attributes } = fm<Record<string, string>>(raw);
  return attributes["title-llm"] || attributes["title"] || docRef.label;
}

// versioned_docs/version-<version> doc-id -> relative-path maps, built lazily
// and cached, used only to resolve the source file for the `title-llm` override.
const docIdToPathMapCache = new Map<string, Map<string, string>>();

function getDocIdToPathMap(version: string): Map<string, string> {
  let docIdToPath = docIdToPathMapCache.get(version);
  if (!docIdToPath) {
    docIdToPath = buildDocIdToPathMap(
      path.join(VERSIONED_DOCS_DIR, `version-${version}`),
    );
    docIdToPathMapCache.set(version, docIdToPath);
  }
  return docIdToPath;
}

function buildDocIdToPathMap(directory: string): Map<string, string> {
  const markdownFiles = globSync("**/*.{md,mdx}", {
    cwd: directory,
    nodir: true,
    ignore: ["**/_*.md", "**/_*.mdx"],
  });

  const docIdToPath = new Map<string, string>();
  for (const filePath of markdownFiles) {
    const docId = normalizePathToDocId(filePath);
    if (!docIdToPath.has(docId)) {
      docIdToPath.set(docId, filePath);
    }
  }
  return docIdToPath;
}

// ----------------------------------------------------------------------------
// Markdown helpers
// ----------------------------------------------------------------------------

// The rendered Markdown leads with the page's `# Title` and uses site-absolute
// links. For the concatenated docs we drop the title (the section adds its own
// `## Title` heading), nest the remaining headings one level deeper, and turn
// links into full URLs so the file stands on its own.
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

async function readFileOrNull(filePath: string): Promise<string | null> {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch {
    return null;
  }
}
