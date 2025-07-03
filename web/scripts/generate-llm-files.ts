import {
  SidebarConfig,
  SidebarItemConfig,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import fm from "front-matter";
import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";
import docsSidebarConfig from "../sidebars.js";

const SITE_ROOT = process.cwd();
const STATIC_DIR = path.join(SITE_ROOT, "static/");
const DOCS_DIR = path.join(SITE_ROOT, "docs/");
const BLOG_DIR = path.join(SITE_ROOT, "blog/");
const GITHUB_RAW_BASE_URL =
  "https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/"; // Use the release branch
const WASP_BASE_URL = "https://wasp.sh/";
const LLM_FULL_FILENAME = "llms-full.txt";
const LLM_OVERVIEW_FILENAME = "llms.txt";
const LLMS_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_OVERVIEW_FILENAME);
const LLMS_FULL_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_FULL_FILENAME);
const CATEGORIES_TO_IGNORE = ["Miscellaneous"];

const OVERVIEW_INTRO_CONTENT = `
# Wasp
Wasp is a full-stack framework with batteries included for React, Node.js, and Prisma.

## Individual documentation sections and guides:
`;
const OVERVIEW_MISC_SECTION_CONTENT = `
## Miscellaneous
- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)
- [Open SaaS -- Wasp's free, open-source SaaS boilerplate starter](https://opensaas.sh)
`;

(async () => await generateFiles())();

/**
 * Main function to generate the LLM-friendly doc files.
 * It orchestrates the process by fetching and processing documentation and blog files,
 * and then writing the final output to the static directory.
 */
async function generateFiles() {
  console.log("Starting LLM file generation...");

  if (!Array.isArray(docsSidebarConfig.docs)) {
    throw new Error(
      "Sidebar configuration for docs is not an array. Please check the sidebars.ts file.",
    );
  }

  const { overviewDocsSection, fullConcatContent } =
    await processDocumentationFiles(docsSidebarConfig.docs);
  const blogSectionContent = await processBlogFiles();

  const llmsTxtContent =
    OVERVIEW_INTRO_CONTENT +
    overviewDocsSection +
    blogSectionContent +
    OVERVIEW_MISC_SECTION_CONTENT;
  const llmsFullTxtContent = fullConcatContent;

  await writeOutputFiles(llmsTxtContent, llmsFullTxtContent);
  console.log("🎉 LLM file generation complete.");
}

/**
 * Processes all documentation files based on the sidebar configuration's order.
 * It builds a map of document IDs to file paths, processes each document,
 * and generates both a summarized overview and a full concatenated string of the content.
 * @param {object[]} sidebarConfigDocs - The `docs` array from the sidebars.js configuration.
 * @returns {{overviewDocsSection: string, fullConcatContent: string}} - An object containing the generated overview section and the full concatenated content.
 */
async function processDocumentationFiles(
  sidebarConfigDocs: SidebarItemConfig[],
): Promise<{ overviewDocsSection: string; fullConcatContent: string }> {
  let overviewDocsSection = "";
  let fullConcatContent = "";

  const orderedDocIds = flattenSidebarItemsToDocIds(sidebarConfigDocs);
  console.log(
    `Found ${orderedDocIds.length} document IDs in sidebar order for processing.`,
  );

  const sidebarOverviewStructure =
    getDocsSidebarCategoryStructure(sidebarConfigDocs);

  const docIdToPathMap = buildDocIdToPathMap(DOCS_DIR);

  const docInfoMap = await populateDocInfoMap(orderedDocIds, docIdToPathMap);

  for (const category of sidebarOverviewStructure) {
    overviewDocsSection += `${category.categoryLabel}\n`;
    for (const docId of category.docIds) {
      if (docInfoMap.has(docId)) {
        const info = docInfoMap.get(docId);
        const relativeToSiteForGithub = path
          .relative(SITE_ROOT, info.absolutePath)
          .replace(/\\/g, "/");
        const githubRawUrl = GITHUB_RAW_BASE_URL + relativeToSiteForGithub;
        overviewDocsSection += `- [${info.title}](${githubRawUrl})\n`;
      } else {
        // Warning already issued during docInfoMap population
      }
    }
  }

  // Build fullConcatContent using sidebar structure with proper heading hierarchy
  for (const category of sidebarOverviewStructure) {
    // Add category header as H1 and separator
    fullConcatContent += `# ${category.categoryLabel}\n\n`;

    for (const docId of category.docIds) {
      if (docInfoMap.has(docId)) {
        const info = docInfoMap.get(docId);
        // Add document title as H2
        fullConcatContent += `## ${info.title}\n\n${info.processedBody}\n\n`;
      }
    }

    // Add category separator
    fullConcatContent += `------\n\n`;
  }
  return { overviewDocsSection, fullConcatContent };
}

/**
 * Scans a directory for markdown files, normalizes their paths to create doc IDs,
 * and returns a map from each unique doc ID to its relative file path.
 * @param {string} directory - The directory to scan for documentation files (e.g., DOCS_DIR).
 * @returns {Map<string, string>} A map where keys are normalized doc IDs and values are the original file paths.
 */
function buildDocIdToPathMap(directory: string): Map<string, string> {
  console.log(`Gathering and processing source files from: ${directory}...`);
  // Use **/*.{md,mdx} to match files at any depth, including directly under docs/
  const allGlobbedRelativeFilePaths = globSync("**/*.{md,mdx}", {
    cwd: directory,
    nodir: true,
    ignore: ["**/_*.md", "**/_*.mdx"],
  });

  const docsRelativePaths = new Map();
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

/**
 * Iterates through an ordered list of doc IDs, reads the corresponding file for each,
 * processes its content, and returns a map containing the processed information for each document.
 * This loop ensures that we process the files in the exact order specified in sidebars.js,
 * regardless of how the filesystem returns them.
 * @param {string[]} orderedDocIds - An array of doc IDs in the desired order.
 * @param {Map<string, string>} docIdToPathMap - A map from doc IDs to their file paths.
 * @returns {Promise<Map<string, DocDetails>>} A promise that resolves to a map where keys are doc IDs
 * and values are objects containing the title, processedBody, and path information.
 */
async function populateDocInfoMap(
  orderedDocIds: string[],
  docIdToPathMap: Map<string, string>,
): Promise<Map<string, DocDetails>> {
  const docInfoMap = new Map<string, DocDetails>();
  for (const docId of orderedDocIds) {
    const relativeDocPath = docIdToPathMap.get(docId);

    if (relativeDocPath) {
      const absolutePath = path.join(DOCS_DIR, relativeDocPath);
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
 * @param {string} filePath - The relative file path to normalize.
 * @returns {string} The normalized doc ID.
 */
function normalizePathToDocId(filePath: string): string {
  // This function converts a file path like 'tutorial/01-create.mdx' into a docId like 'tutorial/create'
  const docIdWithoutExtAndIndex = filePath
    .replace(/\.(mdx|md)$/, "")
    .replace(/\/index$/, "");

  const pathSegments = docIdWithoutExtAndIndex.split("/");
  const lastSegment = pathSegments.pop(); // This will be the filename or last directory name

  // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
  const cleanedLastSegment = lastSegment.replace(/^\d+[-.]/, "");

  let docId;
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
 * @param {SidebarItemConfig[]} sidebarTopLevelItems - The top-level items from the sidebar config.
 * @returns {StructuredOverview[]} A structured array of objects, each containing a categoryLabel and its array of docIds.
 */
function getDocsSidebarCategoryStructure(
  sidebarConfigDocs: SidebarItemConfig[],
): StructuredOverview[] {
  const structuredOverview: StructuredOverview[] = [];

  for (const topItem of sidebarConfigDocs) {
    if (
      typeof topItem !== "string" &&
      topItem.type === "category" &&
      topItem.label &&
      topItem.items &&
      !CATEGORIES_TO_IGNORE.includes(topItem.label)
    ) {
      const docIdsWithinCategory = flattenSidebarItemsToDocIds(topItem.items);
      if (docIdsWithinCategory.length > 0) {
        structuredOverview.push({
          categoryLabel: topItem.label,
          docIds: docIdsWithinCategory,
        });
      }
    }
  }
  return structuredOverview;
}

/**
 * Recursively traverses the sidebar configuration to produce a flat, ordered list of document IDs.
 * @param {SidebarItemConfig[]} items - An array of sidebar items (strings or objects).
 * @returns {string[]} A flat array of doc ID strings.
 */
function flattenSidebarItemsToDocIds(sidebarConfig: SidebarConfig): string[] {
  let paths = [];
  if (!sidebarConfig) return paths;
  // We need to iterate over the sidebarConfig, but it can be an array or an object.
  // The types are defined in a weird way so that `items` can be an object (but we don't use that here).
  if (!Array.isArray(sidebarConfig)) {
    return paths;
  }
  for (const item of sidebarConfig) {
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

/**
 * Processes all blog post files, extracting their title, date, and URL.
 * It returns a markdown-formatted string of the blog posts, sorted by date.
 * @returns {Promise<string>} A promise that resolves to a markdown string listing the blog posts.
 */
async function processBlogFiles(): Promise<string> {
  let blogSectionContent = `## Blogposts\n`;
  const blogPostFiles = globSync("*.{md,mdx}", {
    cwd: BLOG_DIR,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "authors.yml", "components/**"],
  });

  const blogPostsData = [];

  for (const file of blogPostFiles) {
    const dateMatch = validateBlogFileDate(file);
    if (!dateMatch) {
      continue;
    }
    const dateString = dateMatch[1];
    const absoluteFilePath = path.join(BLOG_DIR, file);
    try {
      const rawContent = await fs.readFile(absoluteFilePath, "utf8");
      const { attributes } = fm(rawContent);

      const title = extractBlogPostTitle(attributes as { title: string }, file);
      const linkPath = constructBlogUrl(file);

      if (linkPath) {
        blogPostsData.push({ title, dateString, linkPath });
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

  blogPostsData.sort((a, b) => b.dateString.localeCompare(a.dateString));

  if (blogPostsData.length > 0) {
    for (const post of blogPostsData) {
      blogSectionContent += `- [${post.title}](${post.linkPath})\n`;
    }
    return blogSectionContent.trim();
  } else {
    return "";
  }
}

/**
 * Validates a blog post filename against the 'YYYY-MM-DD-slug' format.
 * Logs a warning for files that don't match and aren't explicitly ignored.
 * @param {string} file - The filename to validate.
 * @returns {RegExpMatchArray | null} The result of the match, or null if it doesn't match.
 */
function validateBlogFileDate(file: string): RegExpMatchArray | null {
  const dateMatch = file.match(/^(\d{4}-\d{2}-\d{2})-.*\.(mdx|md)$/);
  if (dateMatch) {
    return dateMatch;
  }

  // If it doesn't match, check if it's a file type we should warn about.
  const isIgnoredType =
    file === "authors.yml" ||
    file.startsWith("_") ||
    file.includes("components/");

  if (!isIgnoredType) {
    console.warn(
      `Skipping file in web/blog, does not match YYYY-MM-DD-name.md(x) naming convention or is an ignored type: ${file}`,
    );
  }
  return null;
}

/**
 * Extracts the title for a blog post.
 * It first checks for a title in the front-matter attributes. If not found,
 * it generates a title from the filename.
 * @param {object} attributes - The front-matter attributes from the blog post file.
 * @param {string} filename - The filename of the blog post.
 * @returns {string} The extracted or generated title.
 */
function extractBlogPostTitle(
  attributes: { title: string },
  filename: string,
): string {
  if (attributes.title) {
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

/**
 * Constructs the absolute URL for a blog post based on its filename.
 * The filename is expected to follow the 'YYYY-MM-DD-slug' format.
 * @param {string} filename - The filename of the blog post.
 * @returns {string|null} The full URL to the blog post, or null if the filename format is invalid.
 */
function constructBlogUrl(filename: string): string | null {
  const fileNoExt = filename.replace(/\.(mdx|md)$/, "");
  const [year, month, day, ...slugParts] = fileNoExt.split("-");
  const slug = slugParts.join("-");
  const blogPath = `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`;
  return blogPath.replace(/\\/g, "/");
}

/**
 * Writes the LLM-friendly content to their respective output files.
 * @param {string} llmsTxtContent - The content for the overview file (llms.txt).
 * @param {string} llmsFullTxtContent - The content for the full concatenated file (llms-full.txt).
 */
async function writeOutputFiles(
  llmsTxtContent: string,
  llmsFullTxtContent: string,
): Promise<void> {
  console.log("Writing output files to static/ ...");

  await fs.writeFile(LLMS_TXT_FILE_PATH, llmsTxtContent.trim(), "utf8");
  console.log(`Generated overview file: ${LLMS_TXT_FILE_PATH}`);

  await fs.writeFile(
    LLMS_FULL_TXT_FILE_PATH,
    llmsFullTxtContent.trim(),
    "utf8",
  );
  console.log(`Generated full concatenated file: ${LLMS_FULL_TXT_FILE_PATH}`);
}

/**
 * Cleans the raw markdown content of a document to make it more suitable for an LLM.
 * This involves removing React/HTML components, import statements, comments, and other non-content elements.
 * It also adjusts heading levels to be consistent with the overall document structure.
 * @param {string} content - The raw markdown string.
 * @returns {string} The cleaned markdown string.
 */
function cleanDocContent(content: string): string {
  if (!content) return "";

  const componentsToReplace = new Set();
  // NOTE: Not sure if this is needed, as LLMs can probably parse the components's meaning from the context.
  // Regex to capture imports from '@site/src/components/Tag'
  // Example: import MyTag from '...' -> MyTag
  // Example: import {Tag1, Tag2 as MyTag2} from '...' -> {Tag1, Tag2 as MyTag2}
  const importRegex =
    /^\s*import\s+(.+?)\s+from\s+['"]@site\/src\/components\/Tag['"]\s*;?\s*$/gm;

  // Helper to extract actual component names (including aliases)
  // Defined inside cleanContent to be self-contained
  function extractNames(specifier) {
    const names = [];
    specifier = specifier.trim();
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
