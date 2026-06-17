import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import { getSiteRoot } from "../site-root";
import { htmlToMarkdown } from "./convert";
import { isMarkdownRoute } from "./markdown-routes";

/**
 * Post-build step: turn the rendered HTML for docs, blog, and resources pages
 * into Markdown served alongside the HTML. Because `trailingSlash` is false,
 * Docusaurus emits `<route>.html` files, so we write `<route>.md` next to each.
 */

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

// Listing, tag, and pagination pages have no single document body.
const SKIP_PATH_PATTERNS = [/\/tags(\/|\.html$)/, /\/page\/\d+/, /\/authors\./];

generateMarkdownFiles().catch((err) => {
  console.error("Failed to generate Markdown files:", err);
  process.exit(1);
});

async function generateMarkdownFiles(): Promise<void> {
  console.log("Generating Markdown files from built HTML...");

  const htmlFiles = findConvertibleHtmlFiles();
  let written = 0;
  let skipped = 0;

  for (const htmlFileRelativePath of htmlFiles) {
    const htmlFileAbsolutePath = path.join(BUILD_DIR, htmlFileRelativePath);
    const html = await fs.readFile(htmlFileAbsolutePath, "utf8");
    const result = htmlToMarkdown(html);
    if (!result) {
      skipped++;
      continue;
    }

    const markdownPath = htmlFileAbsolutePath.replace(/\.html$/, ".md");
    await fs.writeFile(markdownPath, result.markdown, "utf8");
    written++;
  }

  console.log(
    `Markdown generation complete: ${written} written, ${skipped} skipped (no content).`,
  );
}

function findConvertibleHtmlFiles(): string[] {
  return globSync("**/*.html", {
    cwd: BUILD_DIR,
    nodir: true,
  }).filter(isConvertibleRoute);
}

function isConvertibleRoute(relativePath: string): boolean {
  if (!isMarkdownRoute(toRoute(relativePath))) {
    return false;
  }
  const normalizedPath = "/" + relativePath.replace(/\\/g, "/");
  // TODO: check
  return !SKIP_PATH_PATTERNS.some((pattern) => pattern.test(normalizedPath));
}

/**
 * Maps a build-relative HTML path to its route.
 *
 * @example "docs/tutorial/create.html" → "/docs/tutorial/create"
 * @example "docs.html" → "/docs"
 */
function toRoute(relativePath: string): string {
  return "/" + relativePath.replace(/\\/g, "/").replace(/\.html$/, "");
}
