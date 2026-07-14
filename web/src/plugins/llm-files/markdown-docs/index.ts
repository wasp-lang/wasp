import fs from "fs/promises";
import path from "path";

import { VFile } from "vfile";
import { LlmDocsContext } from "../context";
import { createDocusaurusHtmlToMarkdownProcessor } from "./html-to-md-processor";
import { htmlFileRelPathHasMarkdownVariant } from "./markdown-routes";

/**
 * Turns the rendered HTML for docs, blog, and resources pages into Markdown
 * served alongside the HTML.
 *
 * We opted for HTML -> MD (instead of MDX -> MD) approach because:
 * - It is more stable. HTML -> MD is a very mature pipeline, MDX is a newer concept.
 * - It is less of a maintenance burden. MDX -> MD requires handling each MDX
 *     feature (e.g. a component) preemptively. HTML -> MD only requires handling
 *     non-MD compliant content (e.g. code block titles).
 * - It is more future proof. HTML and Markdown are not prone to changes.
 */
export async function generateMarkdownFilesForValidHtmlFiles(
  context: LlmDocsContext,
): Promise<void> {
  console.log("Generating markdown files from built HTML...");
  const { outDir, baseUrl } = context;

  const markdownDocsIndexHeader = buildMarkdownDocsIndexHeader(baseUrl);
  const htmlToMarkdown = createDocusaurusHtmlToMarkdownProcessor(context);

  const htmlFilesAbsPaths = await findConvertibleHtmlFiles(outDir);
  for (const htmlFileAbsPath of htmlFilesAbsPaths) {
    const htmlContent = await fs.readFile(htmlFileAbsPath, "utf8");
    const htmlFile = new VFile({
      path: htmlFileAbsPath,
      value: htmlContent,
    });

    const markdownContent = htmlToMarkdown(htmlFile);
    const markdownConentWithIndex = markdownDocsIndexHeader + markdownContent;
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    await fs.writeFile(markdownFileAbsPath, markdownConentWithIndex, "utf8");
  }
  console.log(
    `Markdown generation complete: generated ${htmlFilesAbsPaths.length} markdown docs from HTML.`,
  );
}

function buildMarkdownDocsIndexHeader(baseUrl: string): string {
  return `\
> Fetch the complete documentation index at: ${baseUrl}/llms.txt
---

`;
}

async function findConvertibleHtmlFiles(outDir: string): Promise<string[]> {
  const absHtmlFilePaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: outDir,
  })) {
    if (htmlFileRelPathHasMarkdownVariant(htmlFileRelPath)) {
      absHtmlFilePaths.push(path.join(outDir, htmlFileRelPath));
    }
  }

  return absHtmlFilePaths;
}
