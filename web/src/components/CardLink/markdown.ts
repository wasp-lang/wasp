// Markdown renderer for CardLink (CardLink/index.tsx). The component renders a
// clickable card with an optional kind label, title, and description; in
// markdown that becomes a single linked line with the description.

import type { MarkdownRenderer } from "../../llm/doc-component";
import { link, paragraph, text } from "../../llm/md";

const KIND_LABELS: Record<string, string> = {
  guide: "guide",
  api: "API reference",
  docs: "docs",
};

export const CardLink: MarkdownRenderer = ({ props }) => {
  const to = String(props.to ?? "");
  const title = String(props.title ?? "");
  if (!to || !title) return [];

  const kindLabel =
    typeof props.kind === "string" ? KIND_LABELS[props.kind] : undefined;
  const prefix = kindLabel ? `${kindLabel}: ` : "";
  const suffix =
    typeof props.description === "string" ? ` — ${props.description}` : "";

  return [paragraph([text(prefix), link(to, [text(title)]), text(suffix)])];
};
