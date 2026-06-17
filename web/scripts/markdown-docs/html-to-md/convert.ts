import * as cheerio from "cheerio";

import { createDocusaurusLlmFriendlyTurndownService } from "./turndown-rules";

const turndownService = createDocusaurusLlmFriendlyTurndownService();

/**
 * Docusaurus content lives in different wrappers depending on the page type:
 * - docs and API reference use `.theme-doc-markdown`
 * - blog and resources posts use a bare `.markdown` inside the article
 */
const MARKDOWN_CONTENT_CONTAINER_SELECTORS = [
  ".theme-doc-markdown",
  "article .markdown",
];

/**
 * Converts a built Docusaurus HTML page to Markdown.
 *
 * @returns null when the page has no recognizable doc content (e.g. the
 * homepage), which means it should not generate a markdown file.
 */
export function htmlToMarkdown(html: string): {
  title: string;
  markdown: string;
} | null {
  const $ = cheerio.load(html);

  const contentContainer = findContentContainer($);
  if (!contentContainer) {
    return null;
  }

  const contentContainerHTML = $.html(contentContainer);
  const markdown = turndownService.turndown(contentContainerHTML).trim();
  if (!markdown) {
    return null;
  }

  return {
    title: extractTitle($, contentContainer),
    markdown,
  };
}

function findContentContainer(
  $: cheerio.CheerioAPI,
): cheerio.Cheerio<cheerio.AnyNode> | null {
  for (const selector of MARKDOWN_CONTENT_CONTAINER_SELECTORS) {
    const containerElement = $(selector).first();
    if (containerElement.length > 0) {
      return containerElement;
    }
  }
  return null;
}

function extractTitle(
  $: cheerio.CheerioAPI,
  contentContainer: cheerio.Cheerio<cheerio.AnyNode>,
): string {
  const h1 = contentContainer.find("h1").first().text().trim();
  if (h1) {
    return h1;
  }
  const documentTitle = $("title").first().text().trim();
  // If the document title is "Page Title | Wasp"; keep only the page part.
  return documentTitle.replace(/\s*\|\s*Wasp\s*$/, "").trim();
}
