import fs from "fs/promises";
import path from "path";

import { SITE_ROOT_DIR } from "../site-root";
import { htmlToMarkdown } from "./convert";
import { isValidMarkdownDocsRoute } from "./markdown-routes";

/**
 * Post-build step: turn the rendered HTML for docs, blog, and resources pages
 * into Markdown served alongside the HTML. Because `trailingSlash` is false,
 * Docusaurus emits `<route>.html` files, so we write `<route>.md` next to each.
 */

const BUILD_DIR = path.join(SITE_ROOT_DIR, "build");
const MARKDOWN_DOCS_INDEX_HEADER = `\
> Fetch the complete documentation index at: https://wasp.sh/llms.txt
---

`;

generateMarkdownFiles().catch((err) => {
  console.error("Failed to generate Markdown files:", err);
  process.exit(1);
});

async function generateMarkdownFiles(): Promise<void> {
  console.log("Generating markdown files from built HTML...");

  const htmlFilesRelPaths = await findConvertibleHtmlFiles();
  let generatedDocs = 0;
  for (const htmlFileRelPath of htmlFilesRelPaths) {
    const htmlFileAbsPath = path.join(BUILD_DIR, htmlFileRelPath);
    const html = await fs.readFile(htmlFileAbsPath, "utf8");

    const markdown = htmlToMarkdown(html);
    const markdownWithIndex = MARKDOWN_DOCS_INDEX_HEADER + markdown;

    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    await fs.writeFile(markdownFileAbsPath, markdownWithIndex, "utf8");
    generatedDocs++;
  }

  console.log(
    `Markdown generation complete: generated ${generatedDocs} markdown docs from HTML.`,
  );
}

async function findConvertibleHtmlFiles(): Promise<string[]> {
  const htmlFileRelPaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: BUILD_DIR,
  })) {
    if (isValidMarkdownDocsRoute(toRoute(htmlFileRelPath))) {
      htmlFileRelPaths.push(htmlFileRelPath);
    }
  }

  return htmlFileRelPaths;
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
      // "YYYY/MM/DD" → "YYYY-MM-DD"
      .replace(/(\d{4})\/(\d{2})\/(\d{2})/g, "$1-$2-$3")
  );
}
