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

type SidebarCategory = {
  label: string;
  docIds: string[];
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
  // to the individual raw markdown files on GitHub.
  const latestWaspVersion = waspVersionsJson[0];
  const blogPostsSection = await buildBlogPostsSection();
  const docsMapsByVersionSection =
    buildDocsMapsByVersionSection(waspVersionsJson);
  const llmsTxtContent = buildLlmsTxtContent(
    docsMapsByVersionSection,
    blogPostsSection,
  );
  await fs.writeFile(path.join(STATIC_DIR, "llms.txt"), llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");

  for (const version of waspVersionsJson) {
    console.log(`Processing version ${version}...`);
    const isLatest = version === latestWaspVersion;
    const docsDir = path.join(VERSIONED_DOCS_DIR, `version-${version}`);
    const sidebarItems = await loadVersionedSidebar(version);
    const categorizedDocs = await loadCategorizedDocs(sidebarItems, docsDir);
    const versionedLlmsTxtContent = buildVersionedLlmsTxtContent(
      version,
      categorizedDocs,
    );
    await fs.writeFile(
      path.join(STATIC_DIR, `llms-${version}.txt`),
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
      path.join(STATIC_DIR, `llms-full-${version}.txt`),
      llmsFullVersionedContent.trim(),
      "utf8",
    );
    console.log(`  Generated: llms-full-${version}.txt`);

    if (isLatest) {
      const llmsFullWithIndex =
        buildFullDocsHeaderWithIndex(version, waspVersionsJson) + fullDocsBody;
      await fs.writeFile(
        path.join(STATIC_DIR, "llms-full.txt"),
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
  docsDir: string,
): Promise<CategorizedDocs> {
  const orderedDocIds = flattenSidebarItemsToDocIds(sidebarItems);
  console.log(
    `Found ${orderedDocIds.length} document IDs in sidebar order for processing.`,
  );

  const sidebarCategories = extractSidebarCategories(sidebarItems);
  const docIdToPathMap = buildDocIdToPathMap(docsDir);
  const docInfoMap = await populateDocInfoMap(
    orderedDocIds,
    docIdToPathMap,
    docsDir,
  );

  const categories = sidebarCategories.map((category) => ({
    label: category.label,
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

async function populateDocInfoMap(
  orderedDocIds: string[],
  docIdToPathMap: Map<string, string>,
  docsDir: string,
): Promise<Map<string, SourceDoc>> {
  const docInfoMap = new Map<string, SourceDoc>();
  for (const docId of orderedDocIds) {
    const relativePath = docIdToPathMap.get(docId);

    if (relativePath) {
      const absolutePath = path.join(docsDir, relativePath);
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
          relativePath,
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
      const docIds = flattenSidebarItemsToDocIds(item.items);
      if (docIds.length > 0) {
        categories.push({
          label: item.label,
          docIds,
        });
      }
    }
  }
  return categories;
}

function flattenSidebarItemsToDocIds(sidebarItems: SidebarConfig): string[] {
  let docIds: string[] = [];
  if (!Array.isArray(sidebarItems)) {
    return docIds;
  }
  for (const item of sidebarItems) {
    if (typeof item === "string") {
      docIds.push(item);
    } else if (item.type === "category" && item.items) {
      docIds = docIds.concat(flattenSidebarItemsToDocIds(item.items));
    } else if (item.type === "autogenerated") {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${item.dirName}' might not be fully processed.`,
      );
    }
  }
  return docIds;
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

function cleanDocContent(content: string): string {
  if (!content) return "";

  const componentsToReplace = new Set<string>();
  // NOTE: Not sure if this is needed, as LLMs can probably parse the component's meaning from context.
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
