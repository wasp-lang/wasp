import axios from 'axios';
import * as Cheerio from 'cheerio';

export async function scrapeUrl(url: string, contentSelector: string = "body"): Promise<{
  title: string;
  content: string;
}> {
  const { data } = await axios.get(url);
  const $ = Cheerio.load(data);
  return {
    title: $("title").text(),
    content: $(contentSelector).text(),
  }
}