import { page, query, route, type Spec } from "@wasp/spec";
import { DOCS } from "./content";
import { readDoc, searchDocs } from "./operations" with { type: "ref" };
import {
  DocOut,
  DocSearchResults,
  ReadDocInput,
  SearchDocsInput,
} from "./schemas" with { type: "ref" };
import { DocPage, DocsIndexPage } from "./ui/DocsPage" with { type: "ref" };

export const docsSpec: Spec = [
  // Dashboard (public), prerendered so agents that fetch them get real HTML
  route("DocsRoute", "/docs", page(DocsIndexPage), { prerender: true }),
  route("DocRoute", "/docs/:slug", page(DocPage), {
    prerender: DOCS.map((d) => `/docs/${d.slug}`),
  }),
  // MCP only: no client route or export.
  query(searchDocs, {
    expose: {
      client: false,
      mcp: {
        title: "Search Linkly docs",
        description:
          "Searches Linkly's documentation and returns the best-matching docs with snippets. Use it whenever you're unsure how a Linkly concept works (slugs, tags, how clicks are counted, time ranges, permissions, confirmations) before calling other tools. Then use read_doc for the full text.",
        input: SearchDocsInput,
        output: DocSearchResults,
        idempotent: true,
      },
    },
  }),
  query(readDoc, {
    expose: {
      client: false,
      mcp: {
        title: "Read a Linkly doc",
        description:
          "Returns the full Markdown of one Linkly doc by its slug (from search_docs results).",
        input: ReadDocInput,
        output: DocOut,
        idempotent: true,
      },
    },
  }),
];
