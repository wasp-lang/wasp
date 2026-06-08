// Markdown renderer for FileExtSwitcher.tsx. The component takes a path like
// `Foo.{js,tsx}` and renders it with the extension for the user's selected
// language. The .md build is the TypeScript view, so we pick the TS extension.

import type { MarkdownRenderer } from "../../llm/doc-component";
import { inlineCode } from "../../llm/md";

export const FileExtSwitcher: MarkdownRenderer = ({ props }) => {
  const path = typeof props.path === "string" ? props.path : "";
  const start = path.lastIndexOf("{");
  if (start === -1) return path ? [inlineCode(path)] : [];

  const [, tsExt = ""] = path.slice(start + 1, -1).split(",");
  return [inlineCode(path.slice(0, start) + tsExt)];
};
