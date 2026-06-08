// Markdown renderers for the tag/pill components in Tag.tsx. These render small
// inline badges on the site; in markdown they become their plain label text.

import type { MarkdownRenderer } from "../../llm/doc-component";
import { text } from "../../llm/md";

const label =
  (value: string): MarkdownRenderer =>
  () => [text(value)];

export const Internal = label("(internal)");
export const Required = label("(required)");
export const Optional = label("(optional)");
