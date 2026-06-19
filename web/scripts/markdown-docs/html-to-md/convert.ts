import * as cheerio from "cheerio";

import { createDocusaurusLlmFriendlyTurndownService } from "./turndowns-service";

const turndownService = createDocusaurusLlmFriendlyTurndownService();

/**
 * Docusaurus content lives in different wrappers depending on the page type:
 * - Docs use `.theme-doc-markdown`.
 * - Blog and resource posts use a bare `.markdown` inside the article.
 * - `blog/` and `resources/` index pages use blog schema.
 */
const MARKDOWN_CONTENT_CONTAINER_SELECTORS = [
  ".theme-doc-markdown",
  "article .markdown",
  'main[itemtype="http://schema.org/Blog"]',
];

/**
 * Converts a built Docusaurus HTML page to Markdown.
 */
export function htmlToMarkdown(html: string): string {
  const $ = cheerio.load(html);
  const contentContainer = findContentContainer($);
  const contentContainerHTML = $.html(contentContainer);

  const markdown = turndownService.turndown(contentContainerHTML).trim();
  if (!markdown) {
    throw Error(
      "Markdown content is null. Most likely a stray document. Please update the `isValidMarkdownDocsRoute` function.",
    );
  }

  return markdown;
}

function findContentContainer(
  $: cheerio.CheerioAPI,
): cheerio.Cheerio<cheerio.AnyNode> {
  for (const selector of MARKDOWN_CONTENT_CONTAINER_SELECTORS) {
    const containerElement = $(selector).first();
    if (containerElement.length > 0) {
      return containerElement;
    }
  }
  throw Error(
    "Unable to find content containers for markdown conversion. Maybe the Docusaurus DOM theme changed?",
  );
}
