// Markdown renderers for the components in TsJsHelpers.tsx.
// Policy: the .md build is the TypeScript view, so we keep TS and drop JS.

import type { MarkdownRenderer } from "../../llm/doc-component";

export const ShowForTs: MarkdownRenderer = ({ children }) => children;

export const ShowForJs: MarkdownRenderer = () => [];
