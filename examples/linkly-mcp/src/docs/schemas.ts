import { z } from "../shared/zod";

export const SearchDocsInput = z.object({
  query: z
    .string()
    .min(1)
    .max(200)
    .describe(
      'What you want to know, in plain words, e.g. "how are clicks counted".',
    ),
  limit: z
    .number()
    .int()
    .min(1)
    .max(20)
    .optional()
    .describe("Maximum number of results, 1–20 (default 5)."),
});
export type SearchDocsInput = z.infer<typeof SearchDocsInput>;

export const ReadDocInput = z.object({
  slug: z
    .string()
    .min(1)
    .max(100)
    .describe('The doc\'s slug, e.g. "how-clicks-are-counted".'),
});
export type ReadDocInput = z.infer<typeof ReadDocInput>;

export const DocSearchResult = z.object({
  slug: z.string().describe("Pass to read_doc for the full text."),
  title: z.string().describe("Doc title."),
  snippet: z.string().describe("A short excerpt around the best match."),
  score: z.number().describe("Relevance score; higher is better."),
  url: z.string().describe("Opens the doc in the Linkly dashboard."),
});

export const DocSearchResults = z.object({
  results: z.array(DocSearchResult).describe("Matching docs, best first."),
});
export type DocSearchResults = z.infer<typeof DocSearchResults>;

export const DocOut = z.object({
  slug: z.string().describe("Doc slug."),
  title: z.string().describe("Doc title."),
  tags: z.array(z.string()).describe("Topics the doc covers."),
  markdown: z.string().describe("The full doc as Markdown."),
  url: z.string().describe("Opens the doc in the Linkly dashboard."),
});
export type DocOut = z.infer<typeof DocOut>;
