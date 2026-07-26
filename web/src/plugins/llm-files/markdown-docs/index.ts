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

  const htmlFilesRelPaths = await findConvertibleHtmlFileRelPaths(outDir);
  for (const htmlFileRelPath of htmlFilesRelPaths) {
    const htmlFileAbsPath = path.join(outDir, htmlFileRelPath);
    const htmlContent = await fs.readFile(htmlFileAbsPath, "utf8");
    const htmlFile = new VFile({
      path: htmlFileAbsPath,
      value: htmlContent,
    });

    const markdownContent = htmlToMarkdown(htmlFile);
    const markdownContentWithIndex = markdownDocsIndexHeader + markdownContent;
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    await fs.writeFile(markdownFileAbsPath, markdownContentWithIndex, "utf8");
  }
  console.log(
    `Markdown generation complete: generated ${htmlFilesRelPaths.length} markdown docs from HTML.`,
  );
}

function buildMarkdownDocsIndexHeader(baseUrl: string): string {
  return `\
> Fetch the complete documentation index at: ${baseUrl}/llms.txt
---

`;
}

async function findConvertibleHtmlFileRelPaths(
  outDir: string,
): Promise<string[]> {
  const htmlFileRelPaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: outDir,
  })) {
    if (htmlFileRelPathHasMarkdownVariant(htmlFileRelPath)) {
      htmlFileRelPaths.push(htmlFileRelPath);
    }
  }

  return htmlFileRelPaths;
}
