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
