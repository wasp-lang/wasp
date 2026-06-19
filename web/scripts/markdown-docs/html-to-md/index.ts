import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import { getSiteRoot } from "../site-root";
import { htmlToMarkdown } from "./convert";
import { isValidMarkdownDocsRoute } from "./markdown-routes";

/**
 * Post-build step: turn the rendered HTML for docs, blog, and resources pages
 * into Markdown served alongside the HTML. Because `trailingSlash` is false,
 * Docusaurus emits `<route>.html` files, so we write `<route>.md` next to each.
 */

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

try {
  await generateMarkdownFiles();
} catch (err) {
  throw new Error("Failed to generate Markdown files", { cause: err });
}

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
  }).filter((htmlFileRelPath) =>
    isValidMarkdownDocsRoute(toRoute(htmlFileRelPath)),
  );
}

/**
 * Maps a build-relative HTML path to its route.
 *
 * @example "docs/tutorial/create.html" → "/docs/tutorial/create"
 * @example "docs.html" → "/docs"
 * @example "blog/2025/12/31/post.html" → "/blog/2025-12-31/post"
 */
function toRoute(htmlFileRelPath: string): string {
  return (
    "/" +
    htmlFileRelPath
      .replace(/\\/g, "/")
      .replace(/\.html$/, "")
      .replace(/(\d{4})\/(\d{2})\/(\d{2})/g, "$1-$2-$3")
  );
}
