import axios from "axios";
import * as Cheerio from "cheerio";
import { NodeHtmlMarkdown } from "node-html-markdown";

export const getContent = createContentGetterWithCache();

export async function getLinksToScrape(startingUrl: string): Promise<{
  links: string[];
}> {
  const startingUrlOrigin = new URL(startingUrl).origin;
  const links: Set<string> = new Set([startingUrl]);
  const { data } = await axios.get(startingUrl);
  const $ = Cheerio.load(data);
  $("a").each((_, element) => {
    const href = $(element).attr("href");
    if (href) {
      const url = new URL(href, startingUrl);
      if (isFile(url)) {
        return;
      }
      if (url.origin === startingUrlOrigin) {
        links.add(cleanUrl(url.href));
      }
    }
  });
  return {
    links: Array.from(links),
  };
}

function cleanUrl(url: string) {
  const urlObject = new URL(url);
  urlObject.hash = "";
  return urlObject.href;
}

function isFile(url: URL): boolean {
  const pathname = url.pathname;
  const lastSegment = pathname.split("/").pop();
  if (!lastSegment) {
    return false;
  }
  const lastSegmentParts = lastSegment.split(".");
  if (lastSegmentParts.length < 2) {
    return false;
  }
  const extension = lastSegmentParts.pop();
  if (!extension) {
    return false;
  }
  return true;
}

function createContentGetterWithCache() {
  const cache = new Map<
    string,
    {
      title: string;
      content: string;
      markdownContent: string;
    }
  >();
  return async function getContent(
    url: string,
    contentSelector: string = "body",
  ): Promise<{
    title: string;
    content: string;
    markdownContent: string;
  }> {
    const key = `${url}::${contentSelector}`;
    if (cache.has(key)) {
      return cache.get(key)!;
    }
    const result = await scrapeUrl(url, contentSelector);
    cache.set(key, result);
    return result;
  };
}

async function scrapeUrl(
  url: string,
  contentSelector: string = "body",
): Promise<{
  title: string;
  content: string;
  markdownContent: string;
}> {
  const { data } = await axios.get(url);
  const $ = Cheerio.load(data);
  return {
    title: $("title").text(),
    content: $(contentSelector).text(),
    markdownContent: htmlToMarkdown($(contentSelector).html()),
  };
}

const nhm = new NodeHtmlMarkdown({
  maxConsecutiveNewlines: 2,
});

function htmlToMarkdown(html: string | null): string {
  if (!html) {
    return "";
  }
  return nhm.translate(html);
}
