// Docs search and read. Public: no principal needed.
import { dashboardUrls } from "../deeplinks/urls";
import { DomainError } from "../shared/errors";
import { frontendUrl, serverUrl } from "../shared/urls";
import { DOCS, type Doc } from "./content";
import type { DocOut } from "./schemas";

const STOPWORDS = new Set(
  "a an and are as at be by can do does for from how i in is it my of on or the to what when where which who why with you your".split(
    " ",
  ),
);

export function tokenize(text: string): string[] {
  return text
    .toLowerCase()
    .split(/[^a-z0-9]+/)
    .filter((t) => t.length > 1 && !STOPWORDS.has(t));
}

/** Loose stemming so "counted" matches "count", "clicks" matches "click". */
function stem(t: string): string {
  return t.replace(/(ing|ed|es|s)$/, "") || t;
}

export function fillTemplate(
  text: string,
  urls = { serverUrl: serverUrl(), appUrl: frontendUrl() },
): string {
  return text
    .replaceAll("{{serverUrl}}", urls.serverUrl)
    .replaceAll("{{appUrl}}", urls.appUrl);
}

/** Title match ×3, tag match ×2, plus term frequency in the body. */
export function scoreDoc(doc: Doc, terms: string[]): number {
  const title = tokenize(doc.title).map(stem);
  const tags = doc.tags.flatMap(tokenize).map(stem);
  const body = tokenize(doc.body).map(stem);
  let score = 0;
  for (const term of terms.map(stem)) {
    score += 3 * title.filter((t) => t === term).length;
    score += 2 * tags.filter((t) => t === term).length;
    score +=
      body.filter((t) => t === term).length /
      Math.max(1, Math.sqrt(body.length / 50));
  }
  return Math.round(score * 100) / 100;
}

function snippetFor(doc: Doc, terms: string[]): string {
  const body = fillTemplate(doc.body)
    .replace(/[#*`|]/g, "")
    .replace(/\s+/g, " ")
    .trim();
  const lower = body.toLowerCase();
  const hit =
    terms
      .map((t) => lower.indexOf(stem(t)))
      .filter((i) => i >= 0)
      .sort((a, b) => a - b)[0] ?? 0;
  const start = Math.max(0, hit - 60);
  return (
    (start > 0 ? "…" : "") +
    body.slice(start, start + 200).trim() +
    (start + 200 < body.length ? "…" : "")
  );
}

export function searchDocs(input: { query: string; limit?: number }) {
  const terms = tokenize(input.query ?? "");
  if (terms.length === 0)
    throw new DomainError(
      "invalid",
      'Pass a search query, for example "how are clicks counted".',
    );
  const limit = Math.min(Math.max(input.limit ?? 5, 1), 20);
  const results = DOCS.map((doc) => ({ doc, score: scoreDoc(doc, terms) }))
    .filter((r) => r.score > 0)
    .sort((a, b) => b.score - a.score || a.doc.slug.localeCompare(b.doc.slug))
    .slice(0, limit)
    .map(({ doc, score }) => ({
      slug: doc.slug,
      title: doc.title,
      snippet: snippetFor(doc, terms),
      score,
      url: dashboardUrls.doc(doc.slug),
    }));
  return { results };
}

export function readDoc(input: { slug: string }): DocOut {
  const doc = DOCS.find((d) => d.slug === input.slug);
  if (!doc) {
    throw new DomainError(
      "not_found",
      `No doc "${input.slug}". Use search_docs to find docs; slugs include ${DOCS.map((d) => d.slug).join(", ")}.`,
    );
  }
  return {
    slug: doc.slug,
    title: doc.title,
    tags: doc.tags,
    markdown: fillTemplate(doc.body),
    url: dashboardUrls.doc(doc.slug),
  };
}
