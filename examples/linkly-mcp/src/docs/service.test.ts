import { describe, expect, it } from "vitest";
import { DomainError } from "../shared/errors";
import { DOCS } from "./content";
import { readDoc, searchDocs } from "./service";

describe("searchDocs", () => {
  it('returns how-clicks-are-counted first for "how are clicks counted"', () => {
    const { results } = searchDocs({ query: "how are clicks counted" });
    expect(results[0]?.slug).toBe("how-clicks-are-counted");
    expect(results[0]?.url).toBe(
      "http://localhost:3000/docs/how-clicks-are-counted",
    );
    expect(results[0]?.snippet.length).toBeGreaterThan(20);
  });

  it.each([
    ["connect claude", "connecting-ai-clients"],
    ["what does links:write allow", "permissions"],
    ["slug already taken", "links-and-slugs"],
    ["delete confirmation expires", "deleting-links"],
    ["90 day range", "analytics-ranges"],
  ])('ranks "%s" → %s first', (query, slug) => {
    expect(searchDocs({ query }).results[0]?.slug).toBe(slug);
  });

  it("respects limit and rejects empty queries", () => {
    expect(searchDocs({ query: "link", limit: 2 }).results).toHaveLength(2);
    expect(() => searchDocs({ query: "  the  " })).toThrow(DomainError);
  });
});

describe("readDoc", () => {
  it("fills in this deployment's URLs", () => {
    const doc = readDoc({ slug: "connecting-ai-clients" });
    expect(doc.markdown).toContain("http://localhost:3001/mcp");
    expect(doc.markdown).not.toContain("{{");
  });

  it("suggests search_docs for unknown slugs", () => {
    expect(() => readDoc({ slug: "nope" })).toThrow(/search_docs/);
  });

  it("has 6–10 docs with unique slugs", () => {
    expect(DOCS.length).toBeGreaterThanOrEqual(6);
    expect(DOCS.length).toBeLessThanOrEqual(10);
    expect(new Set(DOCS.map((d) => d.slug)).size).toBe(DOCS.length);
  });
});
