import type {
  SidebarConfig,
  SidebarItemConfig,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import waspVersionsJson from "../versions.json";
import { DOCS_FOOTER_PATTERN } from "./html-to-md/footer";
import { loadPermalinkMaps, normalizePathToDocId } from "./permalinks";

const SITE_ROOT = process.cwd();
// Output and doc bodies both come from the build: this script runs after
// `docusaurus build`, reads the Markdown that scripts/html-to-md generated next
// to each HTML page, and writes the llms*.txt files into the build output.
const BUILD_DIR = path.join(SITE_ROOT, "build");
const VERSIONED_DOCS_DIR = path.join(SITE_ROOT, "versioned_docs/");
const VERSIONED_SIDEBARS_DIR = path.join(SITE_ROOT, "versioned_sidebars/");
const BLOG_DIR = path.join(SITE_ROOT, "blog/");
const WASP_BASE_URL = "https://wasp.sh/";
const CATEGORIES_TO_IGNORE = ["Miscellaneous"];
const WASP_VERSIONS = new Set<string>(waspVersionsJson);

const LLMS_TXT_INTRO = `# Wasp

> Wasp is a full-stack web framework with batteries included for React, Node.js, and Prisma.
> It handles auth, database, routing, deployment, and more out of the box,
> letting you build production-ready apps faster with less boilerplate.`;

const LLMS_TXT_RESOURCES = `## Other Resources
- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)
- [Open SaaS - Free, Open-Source SaaS Boilerplate built on Wasp](https://opensaas.sh)
- [Wasp GitHub](https://github.com/wasp-lang/wasp)
`;

type SourceDoc = {
  title: string;
  processedBody: string;
  docUrl: string;
};

type DocMapEntry = {
  title: string;
  docUrl: string;
  processedBody: string;
};

type CategorizedDocs = {
  categories: Array<{
    label: string;
    docs: DocMapEntry[];
  }>;
};

type DocRef = {
  docId: string;
  // The versioned_docs/version-<version> folder the doc is read from.
  version: string;
};

type SidebarCategory = {
  label: string;
  docRefs: DocRef[];
};

type BlogPost = {
  title: string;
  fileDate: string;
  linkPath: string;
};

generateFiles().catch((err) => {
  console.error("Failed to generate LLM files:", err);
  process.exit(1);
});

async function generateFiles() {
  console.log("Starting LLM file generation...");

  if (!waspVersionsJson || waspVersionsJson.length === 0) {
    throw new Error("No versions found in versions.json");
  }
  // Our llms.txt URL is the entry point for the LLM.
  // It contains a section with links to llms-{version}.txt files
  // which are versioned documentation maps that link
  // to the individual Markdown pages served on wasp.sh (the `.md` variant).
  const latestWaspVersion = waspVersionsJson[0];
  const blogPostsSection = await buildBlogPostsSection();
  const docsMapsByVersionSection =
    buildDocsMapsByVersionSection(waspVersionsJson);
  const llmsTxtContent = buildLlmsTxtContent(
    docsMapsByVersionSection,
    blogPostsSection,
  );
  await fs.writeFile(path.join(BUILD_DIR, "llms.txt"), llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");

  for (const version of waspVersionsJson) {
    console.log(`Processing version ${version}...`);
    const isLatest = version === latestWaspVersion;
    const sidebarItems = await loadVersionedSidebar(version);
    const categorizedDocs = await loadCategorizedDocs(sidebarItems, version);
    const versionedLlmsTxtContent = buildVersionedLlmsTxtContent(
      version,
      categorizedDocs,
    );
    await fs.writeFile(
      path.join(BUILD_DIR, `llms-${version}.txt`),
      versionedLlmsTxtContent,
      "utf8",
    );
    console.log(`  Generated: llms-${version}.txt`);

    // We also generate a full concatenated documentation file, llms-full.txt,
    // for the latest version only with a section containing links
    // to each llms-full-{version}.txt file.
    const fullDocsBody = buildFullDocsBody(categorizedDocs);
    const llmsFullVersionedContent =
      buildFullDocsHeader(version) + fullDocsBody;
    await fs.writeFile(
      path.join(BUILD_DIR, `llms-full-${version}.txt`),
      llmsFullVersionedContent.trim(),
      "utf8",
    );
    console.log(`  Generated: llms-full-${version}.txt`);

    if (isLatest) {
      const llmsFullWithIndex =
        buildFullDocsHeaderWithIndex(version, waspVersionsJson) + fullDocsBody;
      await fs.writeFile(
        path.join(BUILD_DIR, "llms-full.txt"),
        llmsFullWithIndex.trim(),
        "utf8",
      );
      console.log(`  Generated: llms-full.txt`);
    }
  }

  console.log("LLM files generation completed successfully.");
}

function buildDocsMapsByVersionSection(versions: string[]): string {
  const latestVersion = versions[0];
  let section = `## Documentation Maps by Version\n*IMPORTANT:* You should run \`wasp version\` to get the installed Wasp CLI version before choosing the correct link.\n`;
  for (const version of versions) {
    const label = version === latestVersion ? `${version} (latest)` : version;
    section += `- [${label}](${WASP_BASE_URL}llms-${version}.txt)\n`;
  }
  return section.trim();
}

function buildFullDocsByVersionSection(versions: string[]): string {
  const latestVersion = versions[0];
  let section = `## Full Documentation by Version\n`;
  for (const version of versions) {
    if (version === latestVersion) {
      continue;
    }
    section += `- [${version}](${WASP_BASE_URL}llms-full-${version}.txt)\n`;
  }
  return section.trim();
}

function buildLlmsTxtContent(
  docsMapsByVersionSection: string,
  blogPostsSection: string,
): string {
  const fullDocsSection = `## Full Concatenated Documentation Files
Use the same URL pattern as the versioned documentation maps: ${WASP_BASE_URL}llms-full-{version}.txt`;

  return [
    LLMS_TXT_INTRO,
    docsMapsByVersionSection,
    fullDocsSection,
    blogPostsSection,
    LLMS_TXT_RESOURCES,
  ].join("\n\n");
}

async function loadCategorizedDocs(
  sidebarItems: SidebarItemConfig[],
  version: string,
): Promise<CategorizedDocs> {
  const sidebarCategories = extractSidebarCategories(sidebarItems, version);

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

function buildVersionedLlmsTxtContent(
  version: string,
  docs: CategorizedDocs,
): string {
  const lines: string[] = [];

  for (const category of docs.categories) {
    lines.push(category.label);
    for (const doc of category.docs) {
      lines.push(`- [${doc.title}](${doc.docUrl})`);
    }
  }

  const documentationMap = `## Documentation Map\n${lines.join("\n")}`;
  return `# Wasp ${version} Documentation\n\n${documentationMap}`;
}

function buildFullDocsHeader(version: string): string {
  return `# Wasp ${version} Full Documentation\n\n`;
}

function buildFullDocsHeaderWithIndex(
  version: string,
  versions: string[],
): string {
  const header = buildFullDocsHeader(version);
  const versionSection = buildFullDocsByVersionSection(versions);
  return (
    header +
    "> This is the full documentation for the latest version of Wasp.\n> For other versions, see the links below.\n\n" +
    versionSection +
    "\n\n---\n\n"
  );
}

function buildFullDocsBody(docs: CategorizedDocs): string {
  let content = "";
  for (const category of docs.categories) {
    content += `# ${category.label}\n\n`;
    for (const doc of category.docs) {
      content += `## ${doc.title}\n\n${doc.processedBody}\n\n`;
    }
    content += `------\n\n`;
  }
  return content;
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

// versioned_docs/version-<version> doc-id -> relative-path maps, built lazily
// and cached because a single version's sidebar can reference migration
// guides that live in many other versions' folders.
const docIdToPathMapCache = new Map<string, Map<string, string>>();
// Resolved docs cached by build Markdown path: the same migration guide is
// referenced by every later version's sidebar, so we only read it once.
const sourceDocCache = new Map<string, SourceDoc | null>();

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

const permalinkMapsByVersion = loadPermalinkMaps(SITE_ROOT);

function getPermalinkMap(version: string): Map<string, string> {
  return permalinkMapsByVersion.get(version) ?? new Map();
}

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
  const withoutFooter = markdown.replace(DOCS_FOOTER_PATTERN, "");
  const withoutTitle = withoutFooter.replace(/^#\s+.*\n+/, "");
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

function extractSidebarCategories(
  sidebarItems: SidebarItemConfig[],
  currentVersion: string,
): SidebarCategory[] {
  const categories: SidebarCategory[] = [];

  for (const item of sidebarItems) {
    if (
      typeof item !== "string" &&
      item.type === "category" &&
      item.label &&
      item.items &&
      !CATEGORIES_TO_IGNORE.includes(item.label)
    ) {
      const docRefs = flattenSidebarItemsToDocRefs(item.items, currentVersion);
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
  for (const item of sidebarItems) {
    if (typeof item === "string") {
      docRefs.push({ docId: item, version: currentVersion });
    } else if (item.type === "category" && item.items) {
      docRefs.push(...flattenSidebarItemsToDocRefs(item.items, currentVersion));
    } else if ((item.type === "doc" || item.type === "ref") && item.id) {
      docRefs.push({ docId: item.id, version: currentVersion });
    } else if (item.type === "link" && item.href) {
      // Migration guides link to other versions' docs in their versioned
      // form (e.g. /docs/0.22/migration-guide), so resolve those too.
      const ref = parseInternalDocHref(item.href);
      if (ref) {
        docRefs.push(ref);
      }
    } else if (item.type === "autogenerated") {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${item.dirName}' might not be fully processed.`,
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

async function buildBlogPostsSection(): Promise<string> {
  const blogPostFiles = globSync("*.{md,mdx}", {
    cwd: BLOG_DIR,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "authors.yml", "components/**"],
  });

  const blogPosts: BlogPost[] = [];

  for (const file of blogPostFiles) {
    const fileDate = parseBlogFileDate(file);
    if (!fileDate) {
      continue;
    }
    const absoluteFilePath = path.join(BLOG_DIR, file);
    try {
      const rawContent = await fs.readFile(absoluteFilePath, "utf8");
      const { attributes } = fm(rawContent);

      const title = extractBlogPostTitle(attributes, file);
      const linkPath = constructBlogUrl(file);

      if (linkPath) {
        blogPosts.push({ title, fileDate, linkPath });
      } else {
        console.warn(
          `Skipping blog post ${file}, does not match YYYY-MM-DD-name.md(x) naming convention`,
        );
      }
    } catch (fileReadError) {
      // This is intentionally not re-thrown to allow the script to continue
      // and generate a partial file, even if some source files have errors.
      console.error(
        `Error reading or processing blog file ${absoluteFilePath}:`,
        fileReadError,
      );
    }
  }

  blogPosts.sort((a, b) => b.fileDate.localeCompare(a.fileDate));

  if (blogPosts.length === 0) {
    return "";
  }

  let section = `## Blogposts\n`;
  for (const post of blogPosts) {
    section += `- [${post.title}](${post.linkPath})\n`;
  }
  return section.trim();
}

function parseBlogFileDate(file: string): string | null {
  const dateMatch = file.match(/^(\d{4}-\d{2}-\d{2})-.*\.(mdx|md)$/);
  if (dateMatch !== null) {
    return dateMatch[1];
  }

  console.warn(
    `Skipping file in web/blog, does not match YYYY-MM-DD-name.md(x) naming convention: ${file}`,
  );
  return null;
}

function extractBlogPostTitle(attributes: unknown, filename: string): string {
  if (
    typeof attributes === "object" &&
    attributes !== null &&
    "title" in attributes &&
    typeof (attributes as { title: unknown }).title === "string"
  ) {
    return (attributes as { title: string }).title;
  }

  let title = path.basename(filename, path.extname(filename));
  title = title.replace(/^\d{4}-\d{2}-\d{2}-/, "");
  title = title.replace(/-/g, " ");
  return title
    .split(" ")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ");
}

function constructBlogUrl(filename: string): string {
  const basename = filename.replace(/\.(mdx|md)$/, "");
  const [year, month, day, ...slugParts] = basename.split("-");
  const slug = slugParts.join("-");
  const blogPath = `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`;
  return blogPath.replace(/\\/g, "/");
}

async function loadVersionedSidebar(
  version: string,
): Promise<SidebarItemConfig[]> {
  const sidebarPath = path.join(
    VERSIONED_SIDEBARS_DIR,
    `version-${version}-sidebars.json`,
  );
  const sidebarContent = await fs.readFile(sidebarPath, "utf8");

  let sidebarConfig: { docs?: unknown };
  try {
    sidebarConfig = JSON.parse(sidebarContent);
  } catch (parseError) {
    const errorMessage =
      parseError instanceof Error ? parseError.message : String(parseError);
    throw new Error(
      `Failed to parse sidebar JSON for version "${version}" at ${sidebarPath}: ${errorMessage}`,
    );
  }

  if (!Array.isArray(sidebarConfig.docs)) {
    throw new Error(
      `Versioned sidebar configuration for ${version} does not have a docs array.`,
    );
  }

  return sidebarConfig.docs;
}
