import type {
  SidebarConfig,
  SidebarItemConfig,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import waspVersionsJson from "../../versions.json";

import type { LlmFile } from "./common";

const GITHUB_RAW_BASE_URL =
  "https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/"; // Use the release branch
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
  absolutePath: string;
  relativePath: string;
};

type DocMapEntry = {
  title: string;
  githubRawUrl: string;
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

type DocsLlmFilesContext = {
  siteDir: string;
  versionedDocsDir: string;
  versionedSidebarsDir: string;
  blogDir: string;
  docIdToPathMapCache: Map<string, Map<string, string>>;
  sourceDocCache: Map<string, SourceDoc | null>;
};

export async function buildDocsLlmFiles(siteDir: string): Promise<LlmFile[]> {
  console.log("Starting LLM file generation...");

  if (!waspVersionsJson || waspVersionsJson.length === 0) {
    throw new Error("No versions found in versions.json");
  }

  const context = createDocsLlmFilesContext(siteDir);
  const files: LlmFile[] = [];
  // Our llms.txt URL is the entry point for the LLM.
  // It contains a section with links to llms-{version}.txt files
  // which are versioned documentation maps that link
  // to the individual raw markdown files on GitHub.
  const latestWaspVersion = waspVersionsJson[0];
  const blogPostsSection = await buildBlogPostsSection(context);
  const docsMapsByVersionSection =
    buildDocsMapsByVersionSection(waspVersionsJson);
  const llmsTxtContent = buildLlmsTxtContent(
    docsMapsByVersionSection,
    blogPostsSection,
  );
  files.push({ fileName: "llms.txt", content: llmsTxtContent });

  for (const version of waspVersionsJson) {
    console.log(`Processing version ${version}...`);
    const isLatest = version === latestWaspVersion;
    const sidebarItems = await loadVersionedSidebar(context, version);
    const categorizedDocs = await loadCategorizedDocs(
      context,
      sidebarItems,
      version,
    );
    const versionedLlmsTxtContent = buildVersionedLlmsTxtContent(
      version,
      categorizedDocs,
    );
    files.push({
      fileName: `llms-${version}.txt`,
      content: versionedLlmsTxtContent,
    });

    // We also generate a full concatenated documentation file, llms-full.txt,
    // for the latest version only with a section containing links
    // to each llms-full-{version}.txt file.
    const fullDocsBody = buildFullDocsBody(categorizedDocs);
    const llmsFullVersionedContent =
      buildFullDocsHeader(version) + fullDocsBody;
    files.push({
      fileName: `llms-full-${version}.txt`,
      content: llmsFullVersionedContent,
    });

    if (isLatest) {
      const llmsFullWithIndex =
        buildFullDocsHeaderWithIndex(version, waspVersionsJson) + fullDocsBody;
      files.push({ fileName: "llms-full.txt", content: llmsFullWithIndex });
    }
  }

  return files;
}

function createDocsLlmFilesContext(siteDir: string): DocsLlmFilesContext {
  return {
    siteDir,
    versionedDocsDir: path.join(siteDir, "versioned_docs"),
    versionedSidebarsDir: path.join(siteDir, "versioned_sidebars"),
    blogDir: path.join(siteDir, "blog"),
    docIdToPathMapCache: new Map(),
    sourceDocCache: new Map(),
  };
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
  ]
    .filter(Boolean)
    .join("\n\n");
}

async function loadCategorizedDocs(
  context: DocsLlmFilesContext,
  sidebarItems: SidebarItemConfig[],
  version: string,
): Promise<CategorizedDocs> {
  const sidebarCategories = extractSidebarCategories(sidebarItems, version);

  const categories: CategorizedDocs["categories"] = [];
  for (const category of sidebarCategories) {
    const docs: DocMapEntry[] = [];
    for (const ref of category.docRefs) {
      const info = await resolveDocRef(context, ref);
      if (!info) {
        continue;
      }
      const relativeToSite = path
        .relative(context.siteDir, info.absolutePath)
        .replace(/\\/g, "/");
      docs.push({
        title: info.title,
        githubRawUrl: GITHUB_RAW_BASE_URL + relativeToSite,
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
      lines.push(`- [${doc.title}](${doc.githubRawUrl})`);
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

function getDocIdToPathMap(
  context: DocsLlmFilesContext,
  version: string,
): Map<string, string> {
  let docIdToPath = context.docIdToPathMapCache.get(version);
  if (!docIdToPath) {
    docIdToPath = buildDocIdToPathMap(
      path.join(context.versionedDocsDir, `version-${version}`),
    );
    context.docIdToPathMapCache.set(version, docIdToPath);
  }
  return docIdToPath;
}

async function resolveDocRef(
  context: DocsLlmFilesContext,
  ref: DocRef,
): Promise<SourceDoc | null> {
  const relativePath = getDocIdToPathMap(context, ref.version).get(ref.docId);
  if (!relativePath) {
    console.warn(
      `Document ID "${ref.docId}" was not found in version ${ref.version} (or it is ignored).`,
    );
    return null;
  }

  const absolutePath = path.join(
    context.versionedDocsDir,
    `version-${ref.version}`,
    relativePath,
  );
  if (context.sourceDocCache.has(absolutePath)) {
    return context.sourceDocCache.get(absolutePath)!;
  }

  try {
    const rawContent = await fs.readFile(absolutePath, "utf8");
    const { attributes, body } = fm(rawContent);
    const title =
      attributes["title-llm"] ||
      attributes["title"] ||
      path.basename(absolutePath, path.extname(absolutePath));
    const sourceDoc: SourceDoc = {
      title,
      processedBody: cleanDocContent(body),
      absolutePath,
      relativePath,
    };
    context.sourceDocCache.set(absolutePath, sourceDoc);
    return sourceDoc;
  } catch (fileReadError) {
    // This is intentionally not re-thrown to allow the script to continue
    // and generate a partial file, even if some source files have errors.
    console.error(
      `Error reading or processing file for docId '${ref.docId}' (version ${ref.version}) at ${absolutePath}:`,
      fileReadError,
    );
    context.sourceDocCache.set(absolutePath, null);
    return null;
  }
}

// e.g., 'tutorial/01-create.mdx' → 'tutorial/create'
// e.g., 'data-model/entities/index.md' → 'data-model/entities'
function normalizePathToDocId(filePath: string): string {
  const normalizedPath = filePath
    .replace(/\.(mdx|md)$/, "")
    .replace(/\/index$/, "");

  const pathSegments = normalizedPath.split("/");
  const filename = pathSegments.pop() ?? ""; // This will be the filename or last directory name

  // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
  const cleanFilename = filename.replace(/^\d+[-.]/, "");

  return pathSegments.length > 0
    ? [...pathSegments, cleanFilename].join("/")
    : cleanFilename;
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

async function buildBlogPostsSection(
  context: DocsLlmFilesContext,
): Promise<string> {
  const blogPostFiles = globSync("*.{md,mdx}", {
    cwd: context.blogDir,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "authors.yml", "components/**"],
  });

  const blogPosts: BlogPost[] = [];

  for (const file of blogPostFiles) {
    const fileDate = parseBlogFileDate(file);
    if (!fileDate) {
      continue;
    }
    const absoluteFilePath = path.join(context.blogDir, file);
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
  context: DocsLlmFilesContext,
  version: string,
): Promise<SidebarItemConfig[]> {
  const sidebarPath = path.join(
    context.versionedSidebarsDir,
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

function cleanDocContent(content: string): string {
  if (!content) return "";

  const componentsToReplace = new Set<string>();
  // Regex to capture imports from '@site/src/components/Tag'
  // e.g. import MyTag from '...' -> MyTag
  // e.g. import {Tag1, Tag2 as MyTag2} from '...' -> {Tag1, Tag2 as MyTag2}
  const importRegex =
    /^\s*import\s+(.+?)\s+from\s+['"]@site\/src\/components\/Tag['"]\s*;?\s*$/gm;

  function extractNames(rawSpecifier: string): string[] {
    const names: string[] = [];
    const specifier = rawSpecifier.trim();
    if (specifier.startsWith("{") && specifier.endsWith("}")) {
      const inner = specifier.substring(1, specifier.length - 1).trim();
      if (inner) {
        inner.split(",").forEach((part) => {
          part = part.trim();
          if (part.includes(" as ")) {
            names.push(part.split(" as ")[1].trim());
          } else {
            names.push(part);
          }
        });
      }
    } else {
      names.push(specifier);
    }
    return names.filter(Boolean);
  }

  for (const importMatch of content.matchAll(importRegex)) {
    for (const name of extractNames(importMatch[1])) {
      componentsToReplace.add(name);
    }
  }

  let cleaned = content;

  // Replace <CompName ... /> with "CompName!" (e.g. <Internal /> -> Internal!)
  componentsToReplace.forEach((compName) => {
    const tagRegex = new RegExp(`<(?:${compName})(\\s+[^>]*)?\\s*/>`, "g");
    cleaned = cleaned.replace(tagRegex, `${compName}!`);
  });

  // Remove all import statements
  cleaned = cleaned.replace(/^import\s+.*(?:from\s+['"].*['"])?;?\s*$/gm, "");
  // Remove JSX comments like {/* TODO: ... */}
  cleaned = cleaned.replace(/^\{\/\*.*\*\/\}\s*$/gm, "");
  // Remove HTML comments like <!-- TODO: ... -->
  cleaned = cleaned.replace(/^<!--.*-->\s*$/gm, "");
  // Collapse 3+ newlines to 2
  cleaned = cleaned.replace(/\n{3,}/g, "\n\n");
  // Increase heading levels (# -> ##, ## -> ###, etc.)
  cleaned = cleaned.replace(/^(#{1,6})\s/gm, "#$1 ");

  return cleaned.trim();
}
