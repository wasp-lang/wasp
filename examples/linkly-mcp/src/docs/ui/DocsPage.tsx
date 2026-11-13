import { useMemo, useState } from "react";
import ReactMarkdown from "react-markdown";
import { Link as RouterLink, useParams } from "react-router";
import { config } from "wasp/client";
import { useOrigin } from "../../app/useOrigin";
import { Badge } from "../../components/ui/badge";
import {
  Card,
  CardDescription,
  CardHeader,
  CardTitle,
} from "../../components/ui/card";
import { Input } from "../../components/ui/input";
import { DOCS } from "../content";
import { fillTemplate, searchDocs } from "../service";

/** Both pages are prerendered: dashboard links stay root-relative until the page hydrates. */
function useDocUrls() {
  return { serverUrl: config.apiUrl.replace(/\/+$/, ""), appUrl: useOrigin() };
}

/** /docs: index with search. */
export function DocsIndexPage() {
  const [query, setQuery] = useState("");
  const urls = useDocUrls();
  const results = useMemo(() => {
    if (!query.trim()) return null;
    try {
      return searchDocs({ query, limit: 10 }).results;
    } catch {
      return [];
    }
  }, [query]);

  return (
    <div className="mx-auto max-w-3xl space-y-6">
      <h1 className="font-heading text-2xl font-semibold">Docs</h1>
      <Input
        type="search"
        aria-label="Search docs"
        placeholder="Search the docs…"
        value={query}
        onChange={(e) => setQuery(e.target.value)}
      />
      <div className="grid gap-3">
        {(
          results ??
          DOCS.map((d) => ({
            slug: d.slug,
            title: d.title,
            snippet: fillTemplate(d.body, urls).split("\n")[0],
          }))
        ).map((r) => (
          <RouterLink key={r.slug} to={`/docs/${r.slug}`} className="block">
            <Card className="hover:bg-muted/50 transition-colors">
              <CardHeader>
                <CardTitle>{r.title}</CardTitle>
                <CardDescription className="line-clamp-2">
                  {r.snippet.replace(/[#*`]/g, "")}
                </CardDescription>
              </CardHeader>
            </Card>
          </RouterLink>
        ))}
        {results?.length === 0 && (
          <p className="text-muted-foreground">No docs match.</p>
        )}
      </div>
    </div>
  );
}

/** /docs/:slug: one doc rendered from Markdown. Honors dashboardUrls.doc(slug). */
export function DocPage() {
  const { slug = "" } = useParams();
  const urls = useDocUrls();
  const doc = DOCS.find((d) => d.slug === slug);
  if (!doc) {
    return (
      <div className="mx-auto max-w-3xl space-y-2">
        <h1 className="font-heading text-2xl font-semibold">Doc not found</h1>
        <RouterLink className="underline" to="/docs">
          Back to docs
        </RouterLink>
      </div>
    );
  }
  return (
    <article className="mx-auto max-w-3xl space-y-4">
      <RouterLink
        className="text-muted-foreground text-sm hover:underline"
        to="/docs"
      >
        ← All docs
      </RouterLink>
      <h1 className="font-heading text-3xl font-semibold">{doc.title}</h1>
      <div className="flex flex-wrap gap-1">
        {doc.tags.map((t) => (
          <Badge key={t} variant="outline">
            {t}
          </Badge>
        ))}
      </div>
      <div className="prose prose-neutral dark:prose-invert max-w-none">
        <ReactMarkdown>{fillTemplate(doc.body, urls)}</ReactMarkdown>
      </div>
    </article>
  );
}
