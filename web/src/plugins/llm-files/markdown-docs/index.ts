import fs from "fs/promises";
import path from "path";

import { VFile } from "vfile";
import { MarkdownDocsContext } from "./context";
import { createDocusaurusHtmlToMarkdownProcessor } from "./html-to-md-processor";
import {
  htmlFileRelPathToRoute,
  htmlFileRelPathHasMarkdownVariant,
} from "./markdown-routes";

/**
 * Base markdown variant of each page, keyed by its route
 * (e.g. "/docs/auth/overview").
 *
 * Base means the pure page content, without additions specific
 * to one output (e.g. the markdown docs index header).
 */
export type MarkdownDocByRoute = Map<string, string>;

/**
 * Turns the rendered HTML for docs, blog, and resources pages into
 * in-memory base markdown docs.
 *
 * We opted for HTML -> MD (instead of MDX -> MD) approach because:
 * - It is more stable. HTML -> MD is a very mature pipeline, MDX is a newer concept.
 * - It is less of a maintenance burden. MDX -> MD requires handling each MDX
 *     feature (e.g. a component) preemptively. HTML -> MD only requires handling
 *     non-MD compliant content (e.g. code block titles).
 * - It is more future proof. HTML and Markdown are not prone to changes.
 */
export async function convertValidHtmlFilesToMarkdownDocs(
  context: MarkdownDocsContext,
): Promise<MarkdownDocByRoute> {
  console.log("Converting built HTML into markdown docs...");
  const { outDir } = context;

  const htmlToMarkdown = createDocusaurusHtmlToMarkdownProcessor(context);

  const markdownDocByRoute: MarkdownDocByRoute = new Map();
  for (const htmlFileRelPath of await findConvertibleHtmlFiles(outDir)) {
    const htmlFileAbsPath = path.join(outDir, htmlFileRelPath);
    const htmlContent = await fs.readFile(htmlFileAbsPath, "utf8");
    const htmlFile = new VFile({
      path: htmlFileAbsPath,
      value: htmlContent,
    });

    markdownDocByRoute.set(
      htmlFileRelPathToRoute(htmlFileRelPath),
      htmlToMarkdown(htmlFile),
    );
  }
  console.log(
    `Markdown conversion complete: converted ${markdownDocByRoute.size} HTML files.`,
  );

  return markdownDocByRoute;
}

/**
 * Writes each markdown doc alongside its HTML variant, with the markdown
 * docs index header prepended.
 */
export async function writeMarkdownDocsFiles(
  context: MarkdownDocsContext,
  markdownDocByRoute: MarkdownDocByRoute,
): Promise<void> {
  const { outDir, baseUrl } = context;

  const markdownDocsIndexHeader = buildMarkdownDocsIndexHeader(baseUrl);

  for (const [route, markdown] of markdownDocByRoute) {
    const markdownFileAbsPath = path.join(outDir, route + ".md");
    const markdownWithIndex = markdownDocsIndexHeader + markdown;

    await fs.writeFile(markdownFileAbsPath, markdownWithIndex, "utf8");
  }

  console.log(
    `Markdown generation complete: generated ${markdownDocByRoute.size} markdown docs.`,
  );
}

function buildMarkdownDocsIndexHeader(baseUrl: string): string {
  return `\
> Fetch the complete documentation index at: ${baseUrl}/llms.txt
---

`;
}

async function findConvertibleHtmlFiles(outDir: string): Promise<string[]> {
  const htmlFileAbsPath: string[] = [];

  for await (const htmlFileRelPath of fs.glob("**/*.html", {
    cwd: outDir,
  })) {
    if (htmlFileRelPathHasMarkdownVariant(htmlFileRelPath)) {
      htmlFileAbsPath.push(path.join(outDir, htmlFileRelPath));
    }
  }

  return htmlFileAbsPath;
}
