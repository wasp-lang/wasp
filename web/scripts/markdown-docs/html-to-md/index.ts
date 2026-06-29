import fs from "fs/promises";
import path from "path";

import { WEB_PROJECT_ROOT_DIR } from "../site-root";
import { htmlToMarkdown } from "./convert";
import { isHtmlFileAValidMarkdownCandidate } from "./markdown-routes";

const BUILD_DIR = path.join(WEB_PROJECT_ROOT_DIR, "build");
const MARKDOWN_DOCS_INDEX_HEADER = `\
> Fetch the complete documentation index at: https://wasp.sh/llms.txt
---

`;

generateMarkdownFiles().catch((err) => {
  console.error("Failed to generate Markdown files:", err);
  process.exit(1);
});

/**
 * Turns the rendered HTML for docs, blog, and resources pages into Markdown
 * served alongside the HTML.
 */
async function generateMarkdownFiles(): Promise<void> {
  console.log("Generating markdown files from built HTML...");

  const htmlFilesRelPaths = await findConvertibleHtmlFiles();
  let generatedDocs = 0;
  for (const htmlFileRelPath of htmlFilesRelPaths) {
    const htmlFileAbsPath = path.join(BUILD_DIR, htmlFileRelPath);
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    console.log("Generating: ", markdownFileAbsPath);

    const html = await fs.readFile(htmlFileAbsPath, "utf8");
    const markdown = htmlToMarkdown(html);
    const markdownWithIndex = MARKDOWN_DOCS_INDEX_HEADER + markdown;

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
    if (isHtmlFileAValidMarkdownCandidate(htmlFileRelPath)) {
      htmlFileRelPaths.push(htmlFileRelPath);
    }
  }

  return htmlFileRelPaths;
}
