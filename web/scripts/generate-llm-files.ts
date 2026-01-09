import type {
  SidebarConfig,
  SidebarItemConfig,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import waspVersionsJson from "../versions.json";

const SITE_ROOT = process.cwd();
const STATIC_DIR = path.join(SITE_ROOT, "static/");
const VERSIONED_DOCS_DIR = path.join(SITE_ROOT, "versioned_docs/");
const VERSIONED_SIDEBARS_DIR = path.join(SITE_ROOT, "versioned_sidebars/");
const BLOG_DIR = path.join(SITE_ROOT, "blog/");
const GITHUB_RAW_BASE_URL =
  "https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/"; // Use the release branch
const WASP_BASE_URL = "https://wasp.sh/";
const CATEGORIES_TO_IGNORE = ["Miscellaneous"];

const LLMS_TXT_INTRO = `# Wasp

> Wasp is a full-stack web framework with batteries included for React, Node.js, and Prisma.
> It handles auth, database, routing, deployment, and more out of the box,
> letting you build production-ready apps faster with less boilerplate.`;

const LLMS_TXT_RESOURCES = `## Other Resources
- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)
- [Open SaaS - Free, Open-Source SaaS Boilerplate built on Wasp](https://opensaas.sh)
- [Wasp GitHub](https://github.com/wasp-lang/wasp)
`;

generateFiles().catch((err) => {
  console.error("Failed to generate LLM files:", err);
  process.exit(1);
});

/**
 * Main function to generate the LLM-friendly doc files.
 * Generates llms.txt as the entry point, llms-{version}.txt doc maps for each version,
 * and llms-full.txt for the latest version only.
 */
async function generateFiles() {
  console.log("Starting LLM file generation...");

  if (!waspVersionsJson || waspVersionsJson.length === 0) {
    throw new Error("No versions found in versions.json");
  }

  // Generate llms.txt entry point file
  const latestWaspVersion = waspVersionsJson[0];
  const blogPostsSection = await processBlogFiles();
  const docsMapsByVersionSection = buildDocsMapsByVersionSection(waspVersionsJson);
  const llmsTxtContent = buildLlmsTxtContent(docsMapsByVersionSection, blogPostsSection);
  await fs.writeFile(path.join(STATIC_DIR, "llms.txt"), llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");

  // Generate all versioned docs map files (llms-*.txt)
  for (const version of waspVersionsJson) {
    console.log(`Processing version ${version}...`);
    const isLatest = version === latestWaspVersion;
    const docsDir = path.join(VERSIONED_DOCS_DIR, `version-${version}`);
    const sidebarItems = await loadVersionedSidebar(version);
    const processedDocs = await gatherDocumentation(sidebarItems, docsDir);
    const versionedDocsMapContent = buildVersionedDocsMapContent(version, processedDocs);
    await fs.writeFile(path.join(STATIC_DIR, `llms-${version}.txt`), versionedDocsMapContent, "utf8");
    console.log(`  Generated: llms-${version}.txt`);

    // Generate llms-full.txt only for latest version
    if (isLatest) {
      const llmsFullTxtContent = buildFullDocumentation(processedDocs);
      await writeLlmsFullTxtFile(llmsFullTxtContent);
    }
  }

  console.log("🎉 LLM files generation completed successfully.");
}

/**
 * Builds the full versioned docs map section for the main llms.txt file.
 * Lists all available versions with their llms-{version}.txt URLs.
 */
function buildDocsMapsByVersionSection(versions: string[]): string {
  const latestVersion = versions[0];
  let section = `## Documentation Maps by Version\n`;
  for (const version of versions) {
    const label = version === latestVersion ? `${version} (latest)` : version;
    section += `- [${label}](${WASP_BASE_URL}llms-${version}.txt)\n`;
  }
  return section.trim();
}

async function writeLlmsFullTxtFile(content: string): Promise<void> {
  const outputPath = path.join(STATIC_DIR, "llms-full.txt");
  await fs.writeFile(outputPath, content.trim(), "utf8");
  console.log(`  Generated: llms-full.txt`);
}

/**
 * Builds the entry point content for the main llms.txt file.
 * This is a lightweight overview with links to versioned docs.
 */
function buildLlmsTxtContent(
  docsMapsByVersionSection: string,
  blogPostsSection: string,
): string {
  const fullDocsSection = `## Full Documentation
- [Complete docs for latest version](${WASP_BASE_URL}llms-full.txt)`;

  return [
    LLMS_TXT_INTRO,
    docsMapsByVersionSection,
    fullDocsSection,
    blogPostsSection,
    LLMS_TXT_RESOURCES,
  ].join("\n\n");
}

/**
 * Gathers and structures documentation from the sidebar configuration.
 * Returns a structured object ready for building various output formats.
 */
async function gatherDocumentation(
  sidebarItems: SidebarItemConfig[],
  docsDir: string,
): Promise<ProcessedDocumentation> {
  const orderedDocIds = flattenSidebarItemsToDocIds(sidebarItems);
  console.log(
    `Found ${orderedDocIds.length} document IDs in sidebar order for processing.`,
  );

  const sidebarStructure = getDocsSidebarCategoryStructure(sidebarItems);
  const docIdToPathMap = buildDocIdToPathMap(docsDir);
  const docInfoMap = await populateDocInfoMap(orderedDocIds, docIdToPathMap, docsDir);

  const categories = sidebarStructure.map((category) => ({
    label: category.categoryLabel,
    docs: category.docIds
      .filter((docId) => docInfoMap.has(docId))
      .map((docId) => {
        const info = docInfoMap.get(docId)!;
        const relativeToSite = path
          .relative(SITE_ROOT, info.absolutePath)
          .replace(/\\/g, "/");
        return {
          title: info.title,
          githubRawUrl: GITHUB_RAW_BASE_URL + relativeToSite,
          processedBody: info.processedBody,
        };
      }),
  }));

  return { categories };
}

/**
 * Builds the complete content for a version-specific docs map file (llms-*.txt).
 * Contains the version header and documentation map (table of contents).
 */
function buildVersionedDocsMapContent(
  version: string,
  docs: ProcessedDocumentation,
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

/**
 * Builds the full documentation content for llms-full.txt.
 */
function buildFullDocumentation(docs: ProcessedDocumentation): string {
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

/**
 * Scans a directory for markdown files, normalizes their paths to create doc IDs,
 * and returns a map from each unique doc ID to its relative file path.
 */
function buildDocIdToPathMap(directory: string): Map<string, string> {
  console.log(`Gathering and processing source files from: ${directory}...`);
  // Use **/*.{md,mdx} to match files at any depth, including directly under docs/
  const allGlobbedRelativeFilePaths = globSync("**/*.{md,mdx}", {
    cwd: directory,
    nodir: true,
    ignore: ["**/_*.md", "**/_*.mdx"],
  });

  const docsRelativePaths = new Map<string, string>();
  for (const filePath of allGlobbedRelativeFilePaths) {
    const docId = normalizePathToDocId(filePath);
    if (!docsRelativePaths.has(docId)) {
      docsRelativePaths.set(docId, filePath);
    }
  }
  return docsRelativePaths;
}

type DocDetails = {
  title: string;
  processedBody: string;
  absolutePath: string;
  relativeDocPath: string;
};

type ProcessedDoc = {
  title: string;
  githubRawUrl: string;
  processedBody: string;
};

type ProcessedDocumentation = {
  categories: Array<{
    label: string;
    docs: ProcessedDoc[];
  }>;
};

/**
 * Iterates through an ordered list of doc IDs, reads the corresponding file for each,
 * processes its content, and returns a map containing the processed information for each document.
 * This loop ensures that we process the files in the exact order specified in sidebars.js,
 * regardless of how the filesystem returns them.
 */
async function populateDocInfoMap(
  orderedDocIds: string[],
  docIdToPathMap: Map<string, string>,
  docsDir: string,
): Promise<Map<string, DocDetails>> {
  const docInfoMap = new Map<string, DocDetails>();
  for (const docId of orderedDocIds) {
    const relativeDocPath = docIdToPathMap.get(docId);

    if (relativeDocPath) {
      const absolutePath = path.join(docsDir, relativeDocPath);
      try {
        const rawContent = await fs.readFile(absolutePath, "utf8");
        const { attributes, body } = fm(rawContent);
        const title =
          attributes["title-llm"] ||
          attributes["title"] ||
          path.basename(absolutePath, path.extname(absolutePath));
        const processedBody = cleanDocContent(body);
        docInfoMap.set(docId, {
          title,
          processedBody,
          absolutePath,
          relativeDocPath,
        });
      } catch (fileReadError) {
        // This is intentionally not re-thrown to allow the script to continue
        // and generate a partial file, even if some source files have errors.
        console.error(
          `Error reading or processing file for docId '${docId}' at ${absolutePath}:`,
          fileReadError,
        );
      }
    } else {
      console.warn(
        `Document ID "${docId}" was not found in orderedDocIds or not processed because it is ignored.`,
      );
    }
  }
  return docInfoMap;
}

/**
 * Normalizes a file path into a standardized document ID.
 * e.g., 'tutorial/01-create.mdx' becomes 'tutorial/create'
 * e.g., 'data-model/entities/index.md' becomes 'data-model/entities'
 */
function normalizePathToDocId(filePath: string): string {
  const docIdWithoutExtAndIndex = filePath
    .replace(/\.(mdx|md)$/, "")
    .replace(/\/index$/, "");

  const pathSegments = docIdWithoutExtAndIndex.split("/");
  const lastSegment = pathSegments.pop() ?? ""; // This will be the filename or last directory name

  // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
  const cleanedLastSegment = lastSegment.replace(/^\d+[-.]/, "");

  let docId: string;
  if (pathSegments.length > 0) {
    docId = [...pathSegments, cleanedLastSegment].join("/");
  } else {
    docId = cleanedLastSegment;
  }
  return docId;
}

type StructuredOverview = {
  categoryLabel: string;
  docIds: string[];
};

/**
 * Returns an ordered structure of the sidebar categories and their docIds.
 * This is used for generating the overview section of the LLM files.
 * It ignores categories that are not relevant for the LLM context (e.g., 'Miscellaneous').
 */
function getDocsSidebarCategoryStructure(
  docsSidebarItems: SidebarItemConfig[],
): StructuredOverview[] {
  const structuredOverview: StructuredOverview[] = [];

  for (const topLvlItem of docsSidebarItems) {
    if (
      typeof topLvlItem !== "string" &&
      topLvlItem.type === "category" &&
      topLvlItem.label &&
      topLvlItem.items &&
      !CATEGORIES_TO_IGNORE.includes(topLvlItem.label)
    ) {
      const docIdsWithinCategory = flattenSidebarItemsToDocIds(
        topLvlItem.items,
      );
      if (docIdsWithinCategory.length > 0) {
        structuredOverview.push({
          categoryLabel: topLvlItem.label,
          docIds: docIdsWithinCategory,
        });
      }
    }
  }
  return structuredOverview;
}

/**
 * Recursively traverses the sidebar configuration to produce a flat, ordered list of document IDs.
 */
function flattenSidebarItemsToDocIds(sidebarItems: SidebarConfig): string[] {
  let paths: string[] = [];
  // `SidebarConfig` type can be a list or an object - we handle only the list
  // case here, as the sidebar is expected to be an array of items.
  if (!Array.isArray(sidebarItems)) {
    return paths;
  }
  for (const item of sidebarItems) {
    if (typeof item === "string") {
      paths.push(item);
    } else if (item.type === "category" && item.items) {
      paths = paths.concat(flattenSidebarItemsToDocIds(item.items));
    } else if (item.type === "autogenerated") {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${item.dirName}' might not be fully processed.`,
      );
    }
  }
  return paths;
}

type BlogPost = {
  title: string;
  fileDate: string;
  linkPath: string;
};

/**
 * Processes all blog post files, extracting their title, date, and URL.
 * It returns a markdown-formatted string of the blog posts, sorted by date.
 */
async function processBlogFiles(): Promise<string> {
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

  let blogSectionContent = `## Blogposts\n`;
  if (blogPosts.length > 0) {
    for (const post of blogPosts) {
      blogSectionContent += `- [${post.title}](${post.linkPath})\n`;
    }
    return blogSectionContent.trim();
  } else {
    return "";
  }
}

/**
 * Tries to parse a blog post filename against the 'YYYY-MM-DD-slug' format.
 * Logs a warning for files that don't match and aren't explicitly ignored.
 */
function parseBlogFileDate(file: string): string | null {
  const dateMatch = file.match(/^(\d{4}-\d{2}-\d{2})-.*\.(mdx|md)$/);
  if (dateMatch !== null) {
    return dateMatch[1];
  }

  // Files not matching the date pattern are already filtered by glob, so just return null
  console.warn(
    `Skipping file in web/blog, does not match YYYY-MM-DD-name.md(x) naming convention: ${file}`,
  );
  return null;
}

/**
 * Extracts the title for a blog post.
 * It first checks for a title in the front-matter attributes. If not found,
 * it generates a title from the filename.
 */
function extractBlogPostTitle(attributes: unknown, filename: string): string {
  if (isAttributesWithTitle(attributes)) {
    return attributes.title;
  }

  // Generate title from filename if not in front-matter
  let title = path.basename(filename, path.extname(filename));
  title = title.replace(/^\d{4}-\d{2}-\d{2}-/, ""); // Remove date prefix
  title = title.replace(/-/g, " "); // Replace hyphens with spaces
  // Capitalize each word
  return title
    .split(" ")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ");
}

function isAttributesWithTitle(
  attributes: unknown,
): attributes is { title: string } {
  return (
    typeof attributes === "object" &&
    attributes !== null &&
    "title" in attributes &&
    typeof (attributes as { title: unknown }).title === "string"
  );
}

/**
 * Constructs the absolute URL for a blog post based on its filename.
 * The filename is expected to follow the 'YYYY-MM-DD-slug' format.
 */
function constructBlogUrl(filename: string): string {
  const fileNoExt = filename.replace(/\.(mdx|md)$/, "");
  const [year, month, day, ...slugParts] = fileNoExt.split("-");
  const slug = slugParts.join("-");
  const blogPath = `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`;
  return blogPath.replace(/\\/g, "/");
}

/**
 * Loads and parses a versioned sidebar configuration file.
 * Returns the docs array from the sidebar config.
 */
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

/**
 * Cleans the raw markdown content of a document to make it more suitable for an LLM.
 * This involves removing React/HTML components, import statements, comments, and other non-content elements.
 * It also adjusts heading levels to be consistent with the overall document structure.
 */
function cleanDocContent(content: string): string {
  if (!content) return "";

  const componentsToReplace = new Set<string>();
  // NOTE: Not sure if this is needed, as LLMs can probably parse the components's meaning from the context.
  // Regex to capture imports from '@site/src/components/Tag'
  // Example: import MyTag from '...' -> MyTag
  // Example: import {Tag1, Tag2 as MyTag2} from '...' -> {Tag1, Tag2 as MyTag2}
  const importRegex =
    /^\s*import\s+(.+?)\s+from\s+['"]@site\/src\/components\/Tag['"]\s*;?\s*$/gm;

  // Helper to extract actual component names (including aliases)
  // Defined inside cleanContent to be self-contained
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
      // Default import
      names.push(specifier);
    }
    return names.filter(Boolean); // Filter out any empty names
  }

  // Scan the original content for these specific imports BEFORE any modifications
  for (const importMatch of content.matchAll(importRegex)) {
    for (const name of extractNames(importMatch[1])) {
      componentsToReplace.add(name);
    }
  }

  let cleaned = content; // Start modifications from original content

  componentsToReplace.forEach((compName) => {
    // Regex to match <CompName ... /> (self-closing with optional attributes)
    // Example: <Internal />, <Internal prop="foo" />, <Internal/>
    const tagRegex = new RegExp(`<(?:${compName})(\\s+[^>]*)?\\s*/>`, "g");
    cleaned = cleaned.replace(tagRegex, `${compName}!`);
  });

  // General import removal (removes all imports, including processed ones from @site/src/components/Tag)
  cleaned = cleaned.replace(/^import\s+.*(?:from\s+['"].*['"])?;?\s*$/gm, "");
  // Remove lines like {/* TODO: ... */}
  cleaned = cleaned.replace(/^\{\/\*.*\*\/\}\s*$/gm, "");
  // Remove lines like <!-- TODO: ... -->
  cleaned = cleaned.replace(/^<!--.*-->\s*$/gm, "");
  // Remove more than two line breaks
  cleaned = cleaned.replace(/\n{3,}/g, "\n\n");

  // Increase heading level by adding an extra # to any existing headings
  cleaned = cleaned.replace(/^(#{1,6})\s/gm, "#$1 ");

  return cleaned.trim();
}
