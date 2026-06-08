// Small mdast builders so component markdown renderers stay readable.
// These return plain mdast nodes that the flatten then stringifies once.

import type {
  Code,
  InlineCode,
  Link,
  List,
  Nodes,
  Paragraph,
  Strong,
  Text,
} from "mdast";

export function text(value: string): Text {
  return { type: "text", value };
}

export function inlineCode(value: string): InlineCode {
  return { type: "inlineCode", value };
}

export function strong(children: Nodes[]): Strong {
  return { type: "strong", children: children as Strong["children"] };
}

export function paragraph(children: Nodes[]): Paragraph {
  return { type: "paragraph", children: children as Paragraph["children"] };
}

export function boldParagraph(value: string): Paragraph {
  return paragraph([strong([text(value)])]);
}

export function link(url: string, children: Nodes[]): Link {
  return {
    type: "link",
    url,
    title: null,
    children: children as Link["children"],
  };
}

export function code(lang: string | null, value: string): Code {
  return { type: "code", lang, meta: null, value };
}

// Unordered list; each item is a list of block nodes (usually one paragraph).
export function bulletList(items: Nodes[][]): List {
  return {
    type: "list",
    ordered: false,
    spread: false,
    children: items.map((children) => ({
      type: "listItem",
      spread: false,
      children: children as List["children"][number]["children"],
    })),
  };
}
