import fs from "fs/promises";
import path from "path";

import { htmlToMarkdown } from "./convert";
import { isHtmlFileAValidMarkdownDocsCandidate } from "./markdown-routes";

/**
 * Turns the rendered HTML for docs, blog, and resources pages into Markdown
 * served alongside the HTML.
 */
export async function generateMarkdownFilesForValidHtmlFiles({
  outDir,
  baseUrl,
}: {
  outDir: string;
  baseUrl: string;
}): Promise<void> {
  console.log("Generating markdown files from built HTML...");

  const markdownDocsIndexHeader = buildMarkdownDocsIndexHeader(baseUrl);

  const relHtmlFilePaths = await findValidHtmlFiles(outDir);
  let generatedDocs = 0;
  for (const relHtmlFilePath of relHtmlFilePaths) {
    const absHtmlFilePath = path.join(outDir, relHtmlFilePath);
    const absMarkdownFilePath = absHtmlFilePath.replace(/\.html$/, ".md");

    const html = await fs.readFile(absHtmlFilePath, "utf8");
    const markdown = htmlToMarkdown(html);
    const markdownWithIndex = markdownDocsIndexHeader + markdown;

    await fs.writeFile(absMarkdownFilePath, markdownWithIndex, "utf8");
    generatedDocs++;
  }

  console.log(
    `Markdown generation complete: generated ${generatedDocs} markdown docs from HTML.`,
  );
}

function buildMarkdownDocsIndexHeader(baseUrl: string): string {
  return `\
> Fetch the complete documentation index at: ${baseUrl}/llms.txt
---

`;
}

async function findValidHtmlFiles(buildDir: string): Promise<string[]> {
  const htmlFileRelPaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: buildDir,
  })) {
    if (isHtmlFileAValidMarkdownDocsCandidate(htmlFileRelPath)) {
      htmlFileRelPaths.push(htmlFileRelPath);
    }
  }

  return htmlFileRelPaths;
}
