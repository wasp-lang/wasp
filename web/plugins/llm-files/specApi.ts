import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import type { LlmFile } from "./common";

const SPEC_API_DOCS_PATH = "docs/api/@wasp.sh/spec";
const SPEC_API_FILE_NAME = "spec-api.txt";
const WASP_SPEC_API_BASE_URL = "https://wasp.sh/docs/api/@wasp.sh/spec";

type SpecApiPage = {
  title: string;
  url: string;
  content: string;
};

export async function buildSpecApiFile(siteDir: string): Promise<LlmFile> {
  const apiPages = await loadSpecApiPages(
    path.join(siteDir, SPEC_API_DOCS_PATH),
  );

  return {
    fileName: SPEC_API_FILE_NAME,
    content: buildSpecApiFileContent(apiPages),
  };
}

async function loadSpecApiPages(
  specApiDocsDir: string,
): Promise<SpecApiPage[]> {
  const markdownFiles = await getOrderedMarkdownFiles(specApiDocsDir);
  if (markdownFiles.length === 0) {
    throw new Error(
      `No generated Spec API Markdown files found in ${specApiDocsDir}`,
    );
  }

  const pages: SpecApiPage[] = [];
  for (const relativePath of markdownFiles) {
    const rawContent = await fs.readFile(
      path.join(specApiDocsDir, relativePath),
      "utf8",
    );
    const content = rewriteMarkdownLinks(rawContent, relativePath);

    pages.push({
      title: extractTitle(content, relativePath),
      url: getSpecApiPageUrl(relativePath),
      content,
    });
  }

  return pages;
}

async function getOrderedMarkdownFiles(
  specApiDocsDir: string,
): Promise<string[]> {
  const allMarkdownFiles = globSync("**/*.md", {
    cwd: specApiDocsDir,
    nodir: true,
  }).sort(compareMarkdownPaths);

  if (!allMarkdownFiles.includes("index.md")) {
    return allMarkdownFiles;
  }

  const orderedFiles = new Set<string>(["index.md"]);
  const indexContent = await fs.readFile(
    path.join(specApiDocsDir, "index.md"),
    "utf8",
  );

  for (const linkedFile of extractMarkdownFileLinks(indexContent, "index.md")) {
    if (allMarkdownFiles.includes(linkedFile)) {
      orderedFiles.add(linkedFile);
    }
  }

  for (const file of allMarkdownFiles) {
    orderedFiles.add(file);
  }

  return [...orderedFiles];
}

function buildSpecApiFileContent(apiPages: SpecApiPage[]): string {
  const apiMap = [
    "## API Pages",
    ...apiPages.map((page) => `- [${page.title}](${page.url})`),
  ].join("\n");

  const apiContent = apiPages
    .map((page) => [`Source: ${page.url}`, "", page.content].join("\n"))
    .join("\n\n---\n\n");

  return [
    "# Wasp Spec API",
    "> API reference for @wasp.sh/spec, the TypeScript package used to define Wasp apps in main.wasp.ts.",
    apiMap,
    "---",
    apiContent,
  ]
    .filter(Boolean)
    .join("\n\n");
}

function extractMarkdownFileLinks(
  content: string,
  sourcePath: string,
): string[] {
  const links: string[] = [];
  const linkRegex = /\[[^\]]*\]\(([^)]+\.md(?:#[^)]+)?)\)/g;
  for (const match of content.matchAll(linkRegex)) {
    const target = match[1];
    if (!isExternalUrl(target)) {
      links.push(resolveMarkdownTarget(sourcePath, target).filePath);
    }
  }
  return links;
}

function rewriteMarkdownLinks(content: string, sourcePath: string): string {
  return content.replace(
    /\[([^\]]+)\]\(([^)]+\.md(?:#[^)]+)?)\)/g,
    (match, label, target) => {
      if (isExternalUrl(target)) {
        return match;
      }

      const resolvedTarget = resolveMarkdownTarget(sourcePath, target);
      const url = getSpecApiPageUrl(
        resolvedTarget.filePath,
        resolvedTarget.hash,
      );
      return `[${label}](${url})`;
    },
  );
}

function resolveMarkdownTarget(
  sourcePath: string,
  target: string,
): { filePath: string; hash: string } {
  const [targetPath, rawHash = ""] = target.split("#");
  const resolvedPath = path
    .normalize(path.join(path.dirname(sourcePath), targetPath))
    .replace(/\\/g, "/");

  return {
    filePath: resolvedPath,
    hash: rawHash ? `#${rawHash}` : "",
  };
}

function getSpecApiPageUrl(relativePath: string, hash = ""): string {
  const routePath = relativePath
    .replace(/\.md$/, "")
    .replace(/(^|\/)index$/, "");
  return routePath
    ? `${WASP_SPEC_API_BASE_URL}/${routePath}${hash}`
    : `${WASP_SPEC_API_BASE_URL}${hash}`;
}

function extractTitle(content: string, relativePath: string): string {
  const titleMatch = content.match(/^#\s+(.+)$/m);
  if (titleMatch) {
    return titleMatch[1];
  }

  return path.basename(relativePath, ".md");
}

function compareMarkdownPaths(a: string, b: string): number {
  if (a === b) return 0;
  if (a === "index.md") return -1;
  if (b === "index.md") return 1;
  return a.localeCompare(b);
}

function isExternalUrl(url: string): boolean {
  return url.startsWith("http://") || url.startsWith("https://");
}
