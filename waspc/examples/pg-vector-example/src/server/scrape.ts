import axios from "axios";
import * as Cheerio from "cheerio";

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

export async function scrapeLinks(
  links: string[],
  contentSelector: string = "body"
): Promise<{
  results: {
    url: string;
    title: string;
    content: string;
  }[];
}> {
  const results = await Promise.all(
    links.map(async (url) => {
      const { title, content } = await getContent(url, contentSelector);
      return {
        url,
        title,
        content,
      };
    })
  );
  return {
    results,
  };
}

function createContentGetterWithCache() {
  const cache = new Map<
    string,
    {
      title: string;
      content: string;
    }
  >();
  return async function getContent(
    url: string,
    contentSelector: string = "body"
  ): Promise<{
    title: string;
    content: string;
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
  contentSelector: string = "body"
): Promise<{
  title: string;
  content: string;
}> {
  const { data } = await axios.get(url);
  const $ = Cheerio.load(data);
  return {
    title: $("title").text(),
    content: $(contentSelector).text(),
  };
}
