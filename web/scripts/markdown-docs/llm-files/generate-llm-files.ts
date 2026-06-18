import type {
  SidebarConfig,
  SidebarItemConfig,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import waspVersions from "../../../versions.json";
import { loadPermalinkMaps, normalizePathToDocId } from "../permalinks";
import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";
import { generateLlmsTxtFile } from "./llmsTxt";

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

const VERSIONED_DOCS_DIR = path.join(SITE_ROOT, "versioned_docs");
const VERSIONED_SIDEBARS_DIR = path.join(SITE_ROOT, "versioned_sidebars");

const DOCS_SIDEBAR_CATEGORIES_TO_IGNORE = ["Miscellaneous"];
const WASP_VERSIONS = new Set<string>(waspVersions);

interface CategorizedDocs {
  categories: Array<{
    label: string;
    docs: DocMapEntry[];
  }>;
}

interface DocMapEntry {
  title: string;
  docUrl: string;
  processedBody: string;
}

interface DocRef {
  docId: string;
  // The versioned_docs/version-<version> folder the doc is read from.
  version: string;
}

// llms.txt - index of llms-{version}.txt, llms-full-{version.txt}, blog posts, and other resources
// llms-{version}.txt - index of that versions docs pages
// llms-full-{version.txt} - that versions all docs pages concatenated together
generateLlmFiles().catch((err) => {
  console.error("Failed to generate LLM files:", err);
  process.exit(1);
});

async function generateLlmFiles() {
  console.log("Starting LLM file generation...");

  await generateLlmsTxtFile(waspVersions);
  for (const waspVersion of waspVersions) {
    generateLlmFilesForVersion(waspVersion, waspVersions);
  }

  console.log("LLM files generation completed successfully.");
}

async function generateLlmFilesForVersion(
  waspVersion: string,
  waspVersions: string[],
): Promise<void> {
  console.log(`Processing version ${waspVersion}...`);

  const categorizedDocs = await loadCategorizedDocs(waspVersion);

  await generateVersionedLlmTxt(waspVersion, categorizedDocs);
  console.log(`  Generated: llms-${waspVersion}.txt`);

  const fullDocsBody = buildFullDocsBody(categorizedDocs);

  await generateVersionedLlmFullTxt(waspVersion, fullDocsBody);
  console.log(`  Generated: llms-full-${waspVersion}.txt`);

  const isLatestWaspVersion = waspVersion === waspVersions[0];
  if (isLatestWaspVersion) {
    generateLatestVersionLLmFullTxt(waspVersion, waspVersions, fullDocsBody);
    console.log(`  Generated: llms-full.txt`);
  }
}

async function generateVersionedLlmTxt(
  waspVersion: string,
  categorizedDocs: CategorizedDocs,
): Promise<void> {
  const lines: string[] = [];
  for (const category of categorizedDocs.categories) {
    lines.push(category.label);
    for (const doc of category.docs) {
      lines.push(`- [${doc.title}](${doc.docUrl})`);
    }
  }
  const docsIndex = `## Documentation Index\n${lines.join("\n")}`;

  const llmsTxtContent = `# Wasp ${waspVersion} Documentation\n\n${docsIndex}`;
  const llmsTxtAbsPath = path.join(BUILD_DIR, `llms-${waspVersion}.txt`);

  await fs.writeFile(llmsTxtAbsPath, llmsTxtContent, "utf8");
}

async function generateVersionedLlmFullTxt(
  waspVersion: string,
  fullDocsBody: string,
): Promise<void> {
  const llmsTxtFullContent = buildFullDocsHeader(waspVersion) + fullDocsBody;
  const llmsTxtFullAbsPath = path.join(
    BUILD_DIR,
    `llms-full-${waspVersion}.txt`,
  );

  await fs.writeFile(llmsTxtFullAbsPath, llmsTxtFullContent, "utf8");
}

async function generateLatestVersionLLmFullTxt(
  waspVersion: string,
  waspVersions: string[],
  fullDocsBody: string,
): Promise<void> {
  const llmsTxtFullContent =
    buildLatestVersionFullDocsHeader(waspVersion, waspVersions) + fullDocsBody;
  const llmsTxtFullAbsPath = path.join(BUILD_DIR, `llms-full.txt`);

  await fs.writeFile(llmsTxtFullAbsPath, llmsTxtFullContent, "utf8");
}

function buildLatestVersionFullDocsHeader(
  waspVersion: string,
  waspVersions: string[],
) {
  return (
    buildFullDocsHeader(waspVersion) +
    "> This is the full documentation for the latest version of Wasp.\n> For other versions, see the links below.\n\n" +
    buildFullDocsIndexSection(waspVersions) +
    "\n\n---\n\n"
  );
}

function buildFullDocsHeader(waspVersion: string) {
  return `# Wasp ${waspVersion} Full Documentation\n\n`;
}

function buildFullDocsIndexSection(waspVersions: string[]): string {
  const latestWaspVersion = waspVersions[0];
  let section = `## Full Documentation by Version\n`;

  section += `- [latest (currently ${latestWaspVersion})](${WASP_BASE_URL}llms-full.txt`;
  for (const waspVersion of waspVersions) {
    section += `- [${waspVersion}](${WASP_BASE_URL}llms-full-${waspVersion}.txt)\n`;
  }
  return section;
}

function buildFullDocsBody(categorizedDocs: CategorizedDocs): string {
  let fullDocsBody = "";
  for (const category of categorizedDocs.categories) {
    fullDocsBody += `# ${category.label}\n\n`;
    for (const doc of category.docs) {
      fullDocsBody += `## ${doc.title}\n\n${doc.processedBody}\n\n`;
    }
    fullDocsBody += `------\n\n`;
  }
  return fullDocsBody;
}

// TODO: refactor code below this line

async function loadCategorizedDocs(
  waspVersion: string,
): Promise<CategorizedDocs> {
  const sidebarItems = await loadVersionedSidebarItems(waspVersion);
  const sidebarCategories = extractSidebarCategories(waspVersion, sidebarItems);

  const categories: CategorizedDocs["categories"] = [];
  for (const category of sidebarCategories) {
    const docs: DocMapEntry[] = [];
    for (const ref of category.docRefs) {
      const info = await resolveDocRef(ref);
      if (!info) {
        continue;
      }
      docs.push({
        title: info.title,
        docUrl: info.docUrl,
        processedBody: info.processedBody,
      });
    }
    categories.push({ label: category.label, docs });
  }

  return { categories };
}

async function loadVersionedSidebarItems(
  version: string,
): Promise<SidebarItemConfig[]> {
  const sidebarPath = path.join(
    VERSIONED_SIDEBARS_DIR,
    `version-${version}-sidebars.json`,
  );
  const sidebarContent = await fs.readFile(sidebarPath, "utf8");

  let sidebarConfig: { docs: SidebarItemConfig[] };
  try {
    sidebarConfig = JSON.parse(sidebarContent);
  } catch (parseError) {
    const errorMessage =
      parseError instanceof Error ? parseError.message : String(parseError);
    throw new Error(
      `Failed to parse sidebar JSON for version "${version}" at ${sidebarPath}: ${errorMessage}`,
    );
  }

  return sidebarConfig.docs;
}

interface SidebarCategory {
  label: string;
  docRefs: DocRef[];
}

function extractSidebarCategories(
  waspVersion: string,
  sidebarItems: SidebarItemConfig[],
): SidebarCategory[] {
  const categories: SidebarCategory[] = [];

  for (const item of sidebarItems) {
    if (
      typeof item !== "string" &&
      item.type === "category" &&
      item.label &&
      item.items &&
      !DOCS_SIDEBAR_CATEGORIES_TO_IGNORE.includes(item.label)
    ) {
      const docRefs = flattenSidebarItemsToDocRefs(item.items, waspVersion);
      if (docRefs.length > 0) {
        categories.push({
          label: item.label,
          docRefs,
        });
      }
    }
  }
  return categories;
}

function flattenSidebarItemsToDocRefs(
  sidebarItems: SidebarConfig,
  currentVersion: string,
): DocRef[] {
  const docRefs: DocRef[] = [];

  if (!Array.isArray(sidebarItems)) {
    return docRefs;
  }

  for (const sidebarItem of sidebarItems) {
    if (typeof sidebarItem === "string") {
      docRefs.push({ docId: sidebarItem, version: currentVersion });
    } else if (sidebarItem.type === "category" && sidebarItem.items) {
      docRefs.push(
        ...flattenSidebarItemsToDocRefs(sidebarItem.items, currentVersion),
      );
    } else if (
      (sidebarItem.type === "doc" || sidebarItem.type === "ref") &&
      sidebarItem.id
    ) {
      docRefs.push({ docId: sidebarItem.id, version: currentVersion });
    } else if (sidebarItem.type === "link" && sidebarItem.href) {
      // Migration guides link to other versions' docs in their versioned
      // form (e.g. /docs/0.22/migration-guide), so resolve those too.
      const ref = parseInternalDocHref(sidebarItem.href);
      if (ref) {
        docRefs.push(ref);
      }
    } else if (sidebarItem.type === "autogenerated") {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${sidebarItem.dirName}' might not be fully processed.`,
      );
    }
  }
  return docRefs;
}

// Parses an internal versioned doc link like "/docs/0.22/migration-guide"
// into a { docId, version } ref. Returns null for external links or
// unversioned "/docs/<docId>" links (which point to the current docs, a
// folder this script does not process).
function parseInternalDocHref(href: string): DocRef | null {
  const match = href.match(/^\/docs\/([^/]+)\/(.+?)\/?(?:#.*)?$/);
  if (!match) {
    return null;
  }
  const [, version, docId] = match;
  if (!WASP_VERSIONS.has(version)) {
    return null;
  }
  return { docId, version };
}

interface SourceDoc {
  title: string;
  processedBody: string;
  docUrl: string;
}

// Resolved docs cached by build Markdown path: the same migration guide is
// referenced by every later version's sidebar, so we only read it once.
const sourceDocCache = new Map<string, SourceDoc | null>();

async function resolveDocRef(ref: DocRef): Promise<SourceDoc | null> {
  // The permalink is authoritative: it accounts for `slug` frontmatter and
  // index-page collapsing that a path-based guess would get wrong.
  const permalink = getPermalinkMap(ref.version).get(ref.docId);
  if (!permalink) {
    console.warn(
      `Document ID "${ref.docId}" was not found in built version ${ref.version} (or it is ignored).`,
    );
    return null;
  }

  // `trailingSlash` is false, so the page at "/docs/x/" is the file
  // "build/docs/x.html" and its Markdown sibling is "build/docs/x.md".
  const route = permalink.replace(/\/+$/, "");
  const mdPath = path.join(BUILD_DIR, route + ".md");
  if (sourceDocCache.has(mdPath)) {
    return sourceDocCache.get(mdPath)!;
  }

  const builtMarkdown = await readFileOrNull(mdPath);
  if (builtMarkdown === null) {
    console.warn(
      `No built Markdown for docId "${ref.docId}" (version ${ref.version}) at ${mdPath}. ` +
        `Was scripts/html-to-md run after the build?`,
    );
    sourceDocCache.set(mdPath, null);
    return null;
  }

  const sourceDoc: SourceDoc = {
    title: await readDocTitle(ref),
    processedBody: processBuiltMarkdown(builtMarkdown),
    docUrl: WASP_BASE_URL + route.slice(1) + ".md",
  };
  sourceDocCache.set(mdPath, sourceDoc);
  return sourceDoc;
}

const permalinkMapsByVersion = loadPermalinkMaps(SITE_ROOT);

function getPermalinkMap(version: string): Map<string, string> {
  return permalinkMapsByVersion.get(version) ?? new Map();
}

function buildDocIdToPathMap(directory: string): Map<string, string> {
  console.log(`Gathering and processing source files from: ${directory}...`);
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

// We still read the title from the source frontmatter so the `title-llm`
// override keeps working; the body itself comes from the rendered Markdown.
async function readDocTitle(ref: DocRef): Promise<string> {
  const relativePath = getDocIdToPathMap(ref.version).get(ref.docId);
  const fallback = ref.docId.split("/").pop() ?? ref.docId;
  if (!relativePath) {
    return fallback;
  }
  const sourcePath = path.join(
    VERSIONED_DOCS_DIR,
    `version-${ref.version}`,
    relativePath,
  );
  const raw = await readFileOrNull(sourcePath);
  if (raw === null) {
    return fallback;
  }
  const { attributes } = fm<Record<string, string>>(raw);
  return attributes["title-llm"] || attributes["title"] || fallback;
}

// versioned_docs/version-<version> doc-id -> relative-path maps, built lazily
// and cached because a single version's sidebar can reference migration
// guides that live in many other versions' folders.
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

async function readFileOrNull(filePath: string): Promise<string | null> {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch {
    return null;
  }
}

// The rendered Markdown leads with the page's `# Title`, ends with the docs
// footer, and uses site-absolute links. For the concatenated docs we drop the
// title (the category section adds its own heading) and the footer (it would
// repeat once per page), nest the remaining headings one level deeper, and turn
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
    (_match, sitePath: string) => `](${WASP_BASE_URL}${sitePath.slice(1)})`,
  );
}
