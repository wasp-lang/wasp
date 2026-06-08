// Markdown renderer for LinkGrid.tsx. The component renders a grid of link
// cards from a `links` prop (and optional `caption`); in markdown that becomes
// a bullet list of links, each with its description.

import type { Nodes } from "mdast";

import type { MarkdownRenderer } from "../../llm/doc-component";
import { bulletList, link, paragraph, text } from "../../llm/md";
import type { LinkInfo } from "./index";

export const LinkGrid: MarkdownRenderer = ({ props }) => {
  const links = Array.isArray(props.links) ? (props.links as LinkInfo[]) : [];
  if (links.length === 0) return [];

  const items = links.map(({ title, description, linkTo }) => {
    const line: Nodes[] = [link(linkTo, [text(title)])];
    if (description) line.push(text(` — ${description}`));
    return [paragraph(line)];
  });

  const caption =
    typeof props.caption === "string" ? [paragraph([text(props.caption)])] : [];

  return [bulletList(items), ...caption];
};
