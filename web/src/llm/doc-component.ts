// The dual-render contract for doc components.
//
// A component implements its normal React `render` for the site, and a
// `toMarkdown` that the .md/llms build uses instead. `toMarkdown` must be a
// pure function of (evaluated props, already-flattened children) and must not
// import React or CSS, because it runs in Node during the build, outside the
// webpack/theme context.
//
// Renderers are collected in `markdown-registry.ts`, keyed by the JSX name the
// component is used under in MDX.

import type { Nodes } from "mdast";

export interface MarkdownArgs {
  // Evaluated attributes: `foo="bar"` -> "bar", `foo={[...]}` -> the array,
  // boolean `foo` -> true. Expressions that can't be evaluated are undefined.
  props: Record<string, unknown>;
  // The component's children, already flattened to markdown nodes.
  children: Nodes[];
}

export type MarkdownRenderer = (args: MarkdownArgs) => Nodes[];
