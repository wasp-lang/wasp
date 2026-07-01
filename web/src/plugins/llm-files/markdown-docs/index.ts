import fs from "fs/promises";
import path from "path";

import { LllmDocsContext } from "../context";
import { createDocusaurusHtmlToMarkdownProcessor } from "./html-to-md-processor";
import { isHtmlFileAValidMarkdownVariantCandidate } from "./markdown-routes";

/**
 * Turns the rendered HTML for docs, blog, and resources pages into Markdown
 * served alongside the HTML.
 *
 * We opted out for HTML -> MD (instead of MDX -> MD) approach because:
 * - It is more stable. HTML -> MD is a very matrue pipeline, MDX is a newer concept.
 * - It is less of a maintenance burden. MDX -> MD requires handling each MDX
 *     feature (e.g. a component) preemptively. HTML -> MD only requires handling
 *     non-MD compliant content (e.g. code block titles).
 * - It is more future proof. HTML and Markdown are not prone to changes.
 */
export async function generateMarkdownFilesForValidHtmlFiles(
  context: LllmDocsContext,
): Promise<void> {
  console.log("Generating markdown files from built HTML...");
  const { outDir, baseUrl } = context;

  const markdownDocsIndexHeader = buildMarkdownDocsIndexHeader(baseUrl);
  const htmlToMarkdown = createDocusaurusHtmlToMarkdownProcessor(context);

  const htmlFilesRelPaths = await findConvertibleHtmlFiles(outDir);
  let generatedDocs = 0;
  for (const htmlFileRelPath of htmlFilesRelPaths) {
    const htmlFileAbsPath = path.join(outDir, htmlFileRelPath);
    const markdownFileAbsPath = htmlFileAbsPath.replace(/\.html$/, ".md");

    console.log("Generating: ", markdownFileAbsPath);

    const html = await fs.readFile(htmlFileAbsPath, "utf8");
    const markdown = htmlToMarkdown(html);
    const markdownWithIndex = markdownDocsIndexHeader + markdown;

    await fs.writeFile(markdownFileAbsPath, markdownWithIndex, "utf8");
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

async function findConvertibleHtmlFiles(outDir: string): Promise<string[]> {
  const htmlFileRelPaths: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: outDir,
  })) {
    if (isHtmlFileAValidMarkdownVariantCandidate(htmlFileRelPath)) {
      htmlFileRelPaths.push(htmlFileRelPath);
    }
  }

  return htmlFileRelPaths;
}
