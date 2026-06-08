// Turns Wasp MDX docs into plain, agent-friendly Markdown.
//
// Two entry points share one flatten/stringify core:
//   - `renderMdxToMarkdown(body)`  parses raw source itself (used by the
//     llms.txt generator, which runs before the Docusaurus build).
//   - `treeToMarkdown(tree, path)` takes an already-parsed mdast tree (used by
//     the `extract-doc-markdown` remark plugin, which taps Docusaurus' own
//     pipeline after its remark plugins have run).
//
// The flatten is purely functional: it never mutates the input tree, so it is
// safe to run on the live tree Docusaurus later compiles to HTML.
//
// Policy: JS/TS variants collapse to the TypeScript view (Wasp's recommended
// language), so the output reads as a single coherent document instead of
// duplicating every snippet and sentence. Local `.md`/`.mdx` partials (imported
// and used as `<Component/>`) are inlined, with `{props.x}` substituted from the
// usage's attributes.

import { readFileSync } from "fs";
import type {
  Code,
  Nodes,
  Paragraph,
  Parent,
  Root,
  RootContent,
  Strong,
  Text,
} from "mdast";
import type {} from "mdast-util-directive"; // registers *Directive node types onto mdast
import type {} from "mdast-util-mdx"; // registers mdxJsx*/mdx* node types onto mdast
import path from "path";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkMdx from "remark-mdx";
import remarkParse from "remark-parse";
import remarkStringify from "remark-stringify";
import { unified } from "unified";

import searchAndReplace from "../remark/search-and-replace";
import { cleanMarkdown } from "./docs-markdown";
import { markdownRegistry } from "./markdown-registry";

type MdxJsxElement = Extract<
  Nodes,
  { type: "mdxJsxFlowElement" | "mdxJsxTextElement" }
>;

// Context threaded through the flatten so partial imports can be resolved
// relative to the file that declared them, with loop protection.
interface FlattenContext {
  dir: string | undefined;
  partials: Map<string, string>; // local component name -> absolute .md(x) path
  visited: ReadonlySet<string>; // absolute partial paths already being inlined
  props: Record<string, string>; // partial props, for evaluating `{props.x}`
  unhandled: Set<string>; // names of dropped components lacking a markdown renderer
}

const ADMONITION_NAMES = new Set([
  "note",
  "tip",
  "info",
  "warning",
  "caution",
  "danger",
  "important",
  "success",
]);

const parser = unified()
  .use(remarkParse)
  .use(remarkMdx)
  .use(remarkGfm)
  .use(remarkDirective); // `:::note` admonitions in partials -> directive nodes

const stringifier = unified()
  .use(remarkStringify, { bullet: "-", fence: "`", fences: true, rule: "-" })
  .use(remarkGfm);

// Parses MDX source and returns clean Markdown. Falls back to the old regex
// cleanup if the source can't be parsed, so doc generation never hard-fails.
// `filePath` (absolute) lets local partials resolve and inline.
export async function renderMdxToMarkdown(
  body: string,
  { filePath }: { filePath?: string } = {},
): Promise<string> {
  if (!body) return "";
  try {
    const tree = parser.parse(preprocessMdx(body)) as Root;
    applySearchAndReplace(tree);
    return treeToMarkdown(tree, filePath);
  } catch (err) {
    console.warn(
      `render-mdx: falling back to regex cleanup (${(err as Error).message})`,
    );
    return cleanMarkdown(body);
  }
}

// Flattens an already-parsed mdast tree to Markdown without mutating it.
// `filePath` is the absolute path of the source file the tree came from.
export function treeToMarkdown(
  tree: Root,
  filePath: string | undefined,
): string {
  const dir = filePath ? path.dirname(filePath) : undefined;
  const context: FlattenContext = {
    dir,
    partials: dir ? collectPartialImports(tree.children, dir) : new Map(),
    visited: new Set(filePath ? [filePath] : []),
    props: {},
    unhandled: new Set(),
  };
  const flattened: Root = {
    type: "root",
    children: transformChildren(tree.children, context) as RootContent[],
  };
  warnUnhandled(context.unhandled, filePath);
  return collapseBlankLines(stringifier.stringify(flattened)).trim();
}

// Surfaces components that were dropped because nothing knows how to render them
// to Markdown, so authors can add a `toMarkdown` (see markdown-registry.ts).
function warnUnhandled(
  unhandled: Set<string>,
  filePath: string | undefined,
): void {
  if (unhandled.size === 0) return;
  const where = filePath ? path.basename(filePath) : "doc";
  console.warn(
    `render-mdx: ${where}: no markdown renderer for ${[...unhandled]
      .sort()
      .join(", ")} (dropped)`,
  );
}

function transformChildren(children: Nodes[], ctx: FlattenContext): Nodes[] {
  return children.flatMap((child) => transformNode(child, ctx));
}

function transformNode(node: Nodes, ctx: FlattenContext): Nodes[] {
  switch (node.type) {
    case "mdxjsEsm":
      return [];
    case "mdxFlowExpression":
    case "mdxTextExpression": {
      // `{props.x}` in a partial evaluates to its prop; anything else (JSX
      // comments, `{frontMatter.x}`, ...) yields no string and is dropped.
      const evaluated = evalExpression(node.value, ctx.props);
      if (evaluated === undefined) return [];
      // A flow expression sits at block level, so wrap its text in a paragraph;
      // a bare phrasing node there would break block separation downstream.
      return node.type === "mdxFlowExpression"
        ? [{ type: "paragraph", children: [text(evaluated)] }]
        : [text(evaluated)];
    }
    case "yaml":
      return []; // frontmatter node Docusaurus leaves in the tree
    case "mdxJsxFlowElement":
    case "mdxJsxTextElement":
      return flattenJsxElement(node, ctx);
    case "code":
      return [stripAutoJsMeta(node)];
    default:
      // Admonitions arrive as containerDirective nodes from Docusaurus.
      if (isDirective(node)) return flattenDirective(node, ctx);
      if (isParent(node)) {
        return [
          {
            ...node,
            children: transformChildren(node.children, ctx),
          } as unknown as Nodes,
        ];
      }
      return [node];
  }
}

function flattenJsxElement(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const result = renderJsxElement(node, ctx);
  // A flow (block-level) element must yield block nodes. If a renderer returned
  // phrasing (a link, image, text, ...), wrap those runs in a paragraph;
  // otherwise a phrasing node at block level breaks block separation downstream.
  return node.type === "mdxJsxFlowElement" ? wrapPhrasingRuns(result) : result;
}

function renderJsxElement(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const name = node.name ?? "";

  // Local `.md` partials imported into this doc take precedence.
  if (ctx.partials.has(name)) return inlinePartial(node, ctx);

  // A component that owns its markdown form (the dual-render contract).
  const renderer = markdownRegistry[name];
  if (renderer) {
    return renderer({
      props: evalProps(node.attributes, ctx.props),
      children: transformChildren(node.children, ctx),
    });
  }

  // Structural built-ins whose markdown is a tree transform, not a component.
  if (name === "Tabs") return flattenTabs(node, ctx);
  if (name === "TabItem") return unwrap(node, ctx); // only non-js-ts groups reach here
  // `<CodeBlock language="sh">{`...`}</CodeBlock>` is the JS way to build a
  // fenced block; recover the string into a real code node.
  if (name === "CodeBlock") return flattenCodeBlock(node, ctx);
  if (name === "Collapse") return flattenCollapse(node, ctx);
  if (name === "img") return flattenImage(node);

  // Unregistered wrappers that carry content (TutorialAction, Layout, ...) still
  // render fine by unwrapping; nothing is lost, so we don't warn.
  if (node.children.length > 0) return unwrap(node, ctx);

  // Unregistered self-closing widget: nothing to unwrap and no renderer, so it's
  // dropped. Record component names (PascalCase) so we can warn the author to add
  // a `toMarkdown`; lowercase HTML voids (br, iframe, ...) are just dropped.
  if (/^[A-Z]/.test(name)) ctx.unhandled.add(name);
  return [];
}

const PHRASING_TYPES = new Set([
  "text",
  "emphasis",
  "strong",
  "delete",
  "inlineCode",
  "link",
  "linkReference",
  "image",
  "imageReference",
  "break",
  "footnoteReference",
]);

// Groups runs of phrasing nodes into paragraphs, leaving block nodes as-is.
function wrapPhrasingRuns(nodes: Nodes[]): Nodes[] {
  const out: Nodes[] = [];
  let run: Nodes[] = [];
  const flush = () => {
    if (run.length) {
      out.push({ type: "paragraph", children: run as Paragraph["children"] });
      run = [];
    }
  };
  for (const node of nodes) {
    if (PHRASING_TYPES.has(node.type)) run.push(node);
    else {
      flush();
      out.push(node);
    }
  }
  flush();
  return out;
}

function flattenImage(node: MdxJsxElement): Nodes[] {
  const url = attrString(node, "src");
  if (!url) return [];
  return [
    { type: "image", url, alt: attrString(node, "alt") ?? null, title: null },
  ];
}

// Evaluates a component's attributes into a props object for its markdown
// renderer: `foo="bar"` -> "bar", `foo={expr}` -> the evaluated value, bare
// `foo` -> true.
function evalProps(
  attributes: MdxJsxElement["attributes"],
  scope: Record<string, string>,
): Record<string, unknown> {
  const props: Record<string, unknown> = {};
  for (const attr of attributes) {
    if (attr.type !== "mdxJsxAttribute") continue; // skip {...spread}
    if (attr.value === null) {
      props[attr.name] = true;
    } else if (typeof attr.value === "string") {
      props[attr.name] = attr.value;
    } else {
      props[attr.name] = evalExpressionRaw(attr.value.value, scope);
    }
  }
  return props;
}

function flattenTabs(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const items = node.children.filter(
    (child): child is MdxJsxElement =>
      (child.type === "mdxJsxFlowElement" ||
        child.type === "mdxJsxTextElement") &&
      child.name === "TabItem",
  );

  if (isJsTsGroup(items)) {
    const tsItem = items.find((item) => tabValue(item) === "ts");
    return tsItem ? unwrap(tsItem, ctx) : items.flatMap((i) => unwrap(i, ctx));
  }

  // Real content tabs (npm/yarn, OS variants): label each and render all.
  return items.flatMap((item) => {
    const label = tabLabel(item);
    const heading: Nodes[] = label ? [boldParagraph(label)] : [];
    return [...heading, ...unwrap(item, ctx)];
  });
}

function isJsTsGroup(items: MdxJsxElement[]): boolean {
  const values = new Set(items.map(tabValue));
  return values.has("js") && values.has("ts");
}

function flattenCodeBlock(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const lang = attrString(node, "language");
  const raw = node.children
    .map((child) =>
      child.type === "mdxFlowExpression" || child.type === "mdxTextExpression"
        ? child.value
        : "",
    )
    .join("")
    .trim();

  if (!raw) return [];

  // The child is a JS string/template expression; evaluate it with props bound
  // so ternaries and interpolation resolve. Fall back to a literal unwrap.
  const value = evalExpression(raw, ctx.props) ?? unquote(raw);
  const code: Code = { type: "code", lang: lang ?? null, value };
  return [code];
}

// Evaluates a doc-authored MDX expression with `props` bound, returning a string
// when it produces one. Build-time only, on trusted repo content. Used to
// resolve `{props.x}` and `<CodeBlock>` template literals; anything that throws
// or isn't string-like yields undefined (the caller then drops it).
function evalExpression(
  expression: string,
  props: Record<string, string>,
): string | undefined {
  const result = evalExpressionRaw(expression, props);
  return typeof result === "string" || typeof result === "number"
    ? String(result)
    : undefined;
}

// Evaluates an MDX expression with `props` bound and returns its raw value.
// Build-time only, on trusted repo content. Anything that throws is undefined.
function evalExpressionRaw(
  expression: string,
  props: Record<string, string>,
): unknown {
  try {
    return new Function("props", `return (${expression});`)(props);
  } catch {
    return undefined;
  }
}

function flattenCollapse(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const title = attrString(node, "title");
  const body = unwrap(node, ctx);
  return title ? [boldParagraph(title), ...body] : body;
}

function flattenDirective(node: Nodes & Parent, ctx: FlattenContext): Nodes[] {
  const name = (node as { name?: string }).name ?? "";
  if (node.type !== "containerDirective" || !ADMONITION_NAMES.has(name)) {
    return transformChildren(node.children, ctx);
  }

  // The label can arrive three ways: a leading title node (Docusaurus'
  // `mdxAdmonitionTitle` or remark-directive's `directiveLabel`), or
  // `hProperties.title` (Docusaurus' inline title). Otherwise use the name.
  const [first, ...rest] = node.children;
  const isLabelNode =
    first &&
    ((first as { type?: string }).type === "mdxAdmonitionTitle" ||
      (first as { data?: { directiveLabel?: boolean } }).data?.directiveLabel);
  const body = isLabelNode ? rest : node.children;

  const hProps = (node.data as { hProperties?: { title?: string } } | undefined)
    ?.hProperties;
  const label =
    (isLabelNode ? nodeText(first) : undefined) ??
    hProps?.title ??
    capitalize(name);

  return [boldParagraph(label), ...transformChildren(body, ctx)];
}

// Concatenates the text content of a node's descendants.
function nodeText(node: Nodes): string | undefined {
  if (node.type === "text") return node.value;
  if (!isParent(node)) return undefined;
  const text = node.children
    .map((child) => nodeText(child as Nodes) ?? "")
    .join("");
  return text || undefined;
}

// Reads, prop-substitutes, parses, and flattens a local `.md`/`.mdx` partial
// referenced as `<Component .../>`.
function inlinePartial(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  const absPath = ctx.partials.get(node.name ?? "")!;
  if (ctx.visited.has(absPath)) return []; // cycle guard

  let raw: string;
  try {
    raw = readFileSync(absPath, "utf8");
  } catch {
    return [];
  }

  const tree = parser.parse(preprocessMdx(stripFrontmatter(raw))) as Root;
  applySearchAndReplace(tree);

  const dir = path.dirname(absPath);
  const childCtx: FlattenContext = {
    dir,
    partials: collectPartialImports(tree.children, dir),
    visited: new Set([...ctx.visited, absPath]),
    props: collectProps(node.attributes),
    unhandled: ctx.unhandled, // share so warnings aggregate to the top doc
  };
  return transformChildren(tree.children, childCtx);
}

function collectProps(
  attributes: MdxJsxElement["attributes"],
): Record<string, string> {
  const props: Record<string, string> = {};
  for (const attr of attributes) {
    if (attr.type === "mdxJsxAttribute" && typeof attr.value === "string") {
      props[attr.name] = attr.value;
    }
  }
  return props;
}

// Scans top-level `import Name from './partial.md'` statements and maps the
// imported name to an absolute path. Only local Markdown imports count.
function collectPartialImports(
  children: Nodes[],
  dir: string,
): Map<string, string> {
  const partials = new Map<string, string>();
  const importRegex =
    /import\s+([A-Z][A-Za-z0-9_]*)\s+from\s+['"](\.[^'"]+\.mdx?)['"]/g;

  for (const child of children) {
    if (child.type !== "mdxjsEsm") continue;
    for (const match of child.value.matchAll(importRegex)) {
      const [, name, relPath] = match;
      // Source often escapes the leading underscore (`./\_partial.md`); undo any
      // such JS string escapes before resolving the real path.
      const unescaped = relPath.replace(/\\(.)/g, "$1");
      partials.set(name, path.resolve(dir, unescaped));
    }
  }
  return partials;
}

function unwrap(node: MdxJsxElement, ctx: FlattenContext): Nodes[] {
  return transformChildren(node.children as Nodes[], ctx);
}

function stripAutoJsMeta(node: Code): Code {
  // `auto-js` is a build-time flag for the auto-js-code remark plugin; it means
  // nothing in rendered output, so drop it from the fence meta.
  if (!node.meta) return node;
  const meta = node.meta.replace(/\s*\bauto-js\b/, "").trim();
  return { ...node, meta: meta || null };
}

// Strips template/string-literal wrapping and collapses simple `${x}` (left
// behind after prop substitution) so CodeBlock children become plain code.
function unquote(value: string): string {
  let result = value.trim();
  const quote = result[0];
  if (
    (quote === "`" || quote === "'" || quote === '"') &&
    result.endsWith(quote)
  ) {
    result = result.slice(1, -1);
  }
  return result.replace(/\$\{([^}]*)\}/g, "$1");
}

function applySearchAndReplace(tree: Root): void {
  const makeTransform = searchAndReplace as unknown as () => (
    tree: Root,
  ) => void;
  makeTransform()(tree);
}

function tabValue(item: MdxJsxElement): string | undefined {
  return attrString(item, "value");
}

function tabLabel(item: MdxJsxElement): string | undefined {
  return attrString(item, "label") ?? attrString(item, "value");
}

function attrString(node: MdxJsxElement, name: string): string | undefined {
  const attr = node.attributes.find(
    (a) => a.type === "mdxJsxAttribute" && a.name === name,
  );
  return attr && typeof attr.value === "string" ? attr.value : undefined;
}

function isParent(node: Nodes): node is Nodes & Parent {
  return "children" in node && Array.isArray((node as Parent).children);
}

function isDirective(node: Nodes): node is Nodes & Parent {
  return (
    (node.type === "containerDirective" ||
      node.type === "leafDirective" ||
      node.type === "textDirective") &&
    isParent(node)
  );
}

function boldParagraph(value: string): Paragraph {
  return { type: "paragraph", children: [strong(value)] };
}

function text(value: string): Text {
  return { type: "text", value };
}

function strong(value: string): Strong {
  return { type: "strong", children: [text(value)] };
}

function capitalize(value: string): string {
  return value.charAt(0).toUpperCase() + value.slice(1);
}

function stripFrontmatter(raw: string): string {
  return raw.replace(/^---\n[\s\S]*?\n---\n?/, "");
}

function collapseBlankLines(markdown: string): string {
  return markdown.replace(/\n{3,}/g, "\n\n");
}

// Normalizes Docusaurus-flavored MDX into something the bare remark-mdx parser
// accepts. Only needed on the standalone path; the remark-plugin path receives
// a tree Docusaurus already normalized.
function preprocessMdx(body: string): string {
  return normalizeAdmonitionTitles(stripHeadingIds(stripHtmlComments(body)));
}

// Docusaurus allows an inline admonition title (`:::note Some title`), which
// plain remark-directive doesn't recognize. Rewrite it to the bracketed label
// form (`:::note[Some title]`) so the directive parses.
function normalizeAdmonitionTitles(body: string): string {
  return body.replace(/^(:{3,}[a-z]+)[ \t]+(\S.*?)\s*$/gm, "$1[$2]");
}

function stripHtmlComments(body: string): string {
  return body.replace(/<!--[\s\S]*?-->/g, "");
}

function stripHeadingIds(body: string): string {
  return body.replace(/^(#{1,6} .*?)\s*\{#[\w-]+\}\s*$/gm, "$1");
}
