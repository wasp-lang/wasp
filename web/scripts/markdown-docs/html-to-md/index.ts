import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import { getSiteRoot } from "../site-root";
import { htmlToMarkdown } from "./convert";

/**
 * Post-build step: turn the rendered HTML for docs, blog, and resources pages
 * into Markdown served alongside the HTML. Because `trailingSlash` is false,
 * Docusaurus emits `<route>.html` files, so we write `<route>.md` next to each.
 */

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

generateMarkdownFiles().catch((err) => {
  console.error("Failed to generate Markdown files:", err);
  process.exit(1);
});

async function generateMarkdownFiles(): Promise<void> {
  console.log("Generating markdown files from built HTML...");

  const htmlFilesRelPaths = findConvertibleHtmlFiles();
  let generatedDocs = 0;
  for (const htmlFileRelPath of htmlFilesRelPaths) {
    const htmlFileAbsPath = path.join(BUILD_DIR, htmlFileRelPath);
    const html = await fs.readFile(htmlFileAbsPath, "utf8");

    const markdown = htmlToMarkdown(html);
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    await fs.writeFile(markdownFileAbsPath, markdown, "utf8");
    generatedDocs++;
  }

  console.log(
    `Markdown generation complete: generated ${generatedDocs} markdown docs from HTML.`,
  );
}

function findConvertibleHtmlFiles(): string[] {
  return globSync("**/*.html", {
    cwd: BUILD_DIR,
    nodir: true,
  }).filter(isValidMarkdownDocsHtmlFile);
}

/**
 * Whether a route is (or sits under) one of the Markdown route prefixes.
 */
export function isValidMarkdownDocsHtmlFile(htmlFileRelPath: string): boolean {
  return (
    htmlFileRelPath === "docs.html" ||
    htmlFileRelPath.startsWith("docs/") ||
    htmlFileRelPath === "blog.html" ||
    isBlogPostHtmlFile(htmlFileRelPath) ||
    htmlFileRelPath === "resources.html" ||
    isResourcesPostHtmlFile(htmlFileRelPath)
  );
}

function isBlogPostHtmlFile(htmlFileRelPath: string) {
  return /blog\/\d{4}\/\d{2}\/\d{2}/.test(htmlFileRelPath);
}

function isResourcesPostHtmlFile(htmlFileRelPath: string) {
  return /resources\/\d{4}\/\d{2}\/\d{2}/.test(htmlFileRelPath);
}
