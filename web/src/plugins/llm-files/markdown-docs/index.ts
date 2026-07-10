import fs from "fs/promises";
import path from "path";

import { MarkdownDocsContext } from "./context";
import { createDocusaurusHtmlToMarkdownProcessor } from "./html-to-md-processor";
import { relHtmlFilePathHasMarkdownVariant } from "./markdown-routes";

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
  context: MarkdownDocsContext,
): Promise<void> {
  console.log("Generating markdown files from built HTML...");
  const { outDir, baseUrl } = context;

  const markdownDocsIndexHeader = buildMarkdownDocsIndexHeader(baseUrl);
  const htmlToMarkdown = createDocusaurusHtmlToMarkdownProcessor(context);

  const htmlFilesAbsPaths = await findConvertibleHtmlFiles(outDir);
  for (const htmlFileAbsPath of htmlFilesAbsPaths) {
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    console.log("Generating: ", markdownFileAbsPath);

    const html = await fs.readFile(htmlFileAbsPath, "utf8");
    const markdown = htmlToMarkdown(html);
    const markdownWithIndex = markdownDocsIndexHeader + markdown;

    await fs.writeFile(markdownFileAbsPath, markdownWithIndex, "utf8");
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
  const htmlFileAbsPaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: outDir,
  })) {
    if (relHtmlFilePathHasMarkdownVariant(htmlFileRelPath)) {
      htmlFileAbsPaths.push(path.join(outDir, htmlFileRelPath));
    }
  }

  return htmlFileAbsPaths;
}
