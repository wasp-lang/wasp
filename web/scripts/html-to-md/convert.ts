import * as cheerio from "cheerio";

import { createTurndownService } from "./turndown-rules";

const turndown = createTurndownService();

// Docusaurus content lives in different wrappers depending on the page type:
// docs and API reference use `.theme-doc-markdown`, blog and resources posts use
// a bare `.markdown` inside the article.
const CONTENT_SELECTORS = [
  ".theme-doc-markdown",
  "article .markdown",
  ".markdown",
];

export type ConversionResult = {
  title: string;
  markdown: string;
};

/**
 * Converts a built Docusaurus HTML page to Markdown.
 *
 * @returns null when the page has no recognizable doc content (e.g. the
 * homepage, listing pages, tag pages), which means it should not get a `.md`.
 */
export function htmlToMarkdown(html: string): ConversionResult | null {
  const $ = cheerio.load(html);

  const container = findContentContainer($);
  if (!container) {
    return null;
  }

  const contentHtml = $.html(container);
  const markdown = turndown.turndown(contentHtml).trim();
  if (!markdown) {
    return null;
  }

  return {
    title: extractTitle($, container),
    markdown: markdown + "\n",
  };
}

function findContentContainer(
  $: cheerio.CheerioAPI,
): cheerio.Cheerio<never> | null {
  for (const selector of CONTENT_SELECTORS) {
    const element = $(selector).first();
    if (element.length > 0) {
      return element as cheerio.Cheerio<never>;
    }
  }
  return null;
}

function extractTitle(
  $: cheerio.CheerioAPI,
  container: cheerio.Cheerio<never>,
): string {
  const h1 = container.find("h1").first().text().trim();
  if (h1) {
    return h1;
  }
  // The document title is "Page Title | Wasp"; keep only the page part.
  const documentTitle = $("title").first().text().trim();
  return documentTitle.replace(/\s*\|\s*Wasp\s*$/, "").trim();
}
