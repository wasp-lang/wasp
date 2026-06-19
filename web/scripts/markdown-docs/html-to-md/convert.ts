import type * as hast from "hast";
import * as hastSelect from "hast-util-select";
import type * as hastToMdast from "hast-util-to-mdast";
import type * as mdast from "mdast";
import type * as mdastDirective from "mdast-util-directive";
import rehypeParse from "rehype-parse";
import rehypeRemark from "rehype-remark";
import remarkDirective from "remark-directive";
import remarkGfm from "remark-gfm";
import remarkStringify from "remark-stringify";
import { unified } from "unified";
import { EXIT, SKIP, visit } from "unist-util-visit";

/**
 * Converts Docusaurus HTML output to Markdown.
 *
 * Docusaurus renders MDX into HTML with theme-specific wrappers that
 * the default conversion mangles, so the handlers below recognize them and emit
 * clean Markdown.
 */
const markdownProcessor = unified()
  .use(rehypeParse)
  .use(rehypeSelectAndCleanDocusaurusContent)
  .use(rehypeRemark, {
    handlers: {
      div(state, element: hast.Element) {
        if (hasClass(element, "theme-code-block")) {
          return docusaurusCodeBlockToMdast(element);
        }
        if (hasClass(element, "theme-admonition")) {
          return docusaurusAdmonitionToMdast(state, element);
        }
        if (hasClass(element, "tabs-container")) {
          return docusaurusTabsToMdast(state, element);
        }
        return state.all(element);
      },
    },
  })
  .use(remarkGfm)
  .use(remarkDirective)
  .use(remarkStringify, {
    bullet: "-",
    emphasis: "*",
    strong: "*",
    fence: "`",
    fences: true,
    rule: "-",
    listItemIndent: "one",
  });

/**
 * Converts a built Docusaurus HTML page to Markdown.
 */
export function htmlToMarkdown(html: string): string {
  const markdown = String(markdownProcessor.processSync(html)).trim();
  if (!markdown) {
    throw Error(
      "Markdown content is null. Most likely a stray document. Please update the `isValidMarkdownDocsRoute` function.",
    );
  }
  return markdown;
}

/**
 * Docusaurus content lives in different wrappers depending on the page type:
 * - Docs use `.theme-doc-markdown`.
 * - Blog and resource posts use a bare `.markdown` inside the article.
 * - `blog/` and `resources/` index pages use blog schema.
 */
const MARKDOWN_CONTENT_CONTAINER_SELECTORS = [
  ".theme-doc-markdown",
  "article .markdown",
  'main[itemtype="http://schema.org/Blog"]',
];

/**
 * Reduces the parsed page to just its content container and drops
 * unnecessary nodes that would otherwise leak into the Markdown.
 */
function rehypeSelectAndCleanDocusaurusContent(): (root: hast.Root) => void {
  return (root: hast.Root): void => {
    const contentContainer = findContentContainer(root);
    const lastCheckedWithBanner = findLastCheckedWithBanner(
      root,
      contentContainer,
    );
    root.children = lastCheckedWithBanner
      ? [lastCheckedWithBanner, contentContainer]
      : [contentContainer];

    visit(root, (node, index, parent) => {
      if (!parent || index === undefined) {
        return;
      }
      // React injects empty `<!-- -->` comments around dynamic values.
      const isComment = node.type === "comment";
      const isStrippable = node.type === "element" && isStrippableElement(node);
      if (isComment || isStrippable) {
        parent.children.splice(index, 1);
        return [SKIP, index];
      }
    });
  };
}

/**
 * Guides render a "Last checked with ..." note right before the
 * content container, and would otherwise be dropped.
 */
function findLastCheckedWithBanner(
  root: hast.Root,
  contentContainer: hast.Element,
): hast.Element | undefined {
  let banner: hast.Element | undefined;
  visit(root, "element", (node, index, parent) => {
    if (node !== contentContainer || !parent || index === undefined) {
      return;
    }
    for (let i = index - 1; i >= 0; i--) {
      const sibling = parent.children[i];
      if (sibling.type !== "element") {
        continue;
      }
      const holdsAdmonition =
        hastSelect.matches(".theme-admonition", sibling) ||
        hastSelect.select(".theme-admonition", sibling) !== undefined;
      if (holdsAdmonition) {
        banner = sibling;
      }
      break; // Only the immediate preceding element sibling can be the banner.
    }
    return EXIT;
  });
  return banner;
}

function findContentContainer(root: hast.Root): hast.Element {
  for (const selector of MARKDOWN_CONTENT_CONTAINER_SELECTORS) {
    const containerElement = hastSelect.select(selector, root);
    if (containerElement) {
      return containerElement;
    }
  }
  throw Error(
    "Unable to find content containers for markdown conversion. Maybe the Docusaurus DOM theme changed?",
  );
}

function isStrippableElement(element: hast.Element): boolean {
  const isHashLink = element.tagName === "a" && hasClass(element, "hash-link");
  const isSecretGeneratorButton =
    element.tagName === "button" &&
    getClassNames(element).some((className) =>
      className.startsWith("generateBtn"),
    );
  return isHashLink || isSecretGeneratorButton;
}

/**
 * Converts a Docusaurus code block to a fenced code block. Lines are separate
 * elements with no newline text nodes between them, so we join token-line
 * elements explicitly. The language lives on the container.
 *
 * @example
 * HTML:
 * ```html
 * <div class="theme-code-block language-ts"><pre><code>
 *   <div class="token-line">const x = 1;</div>
 *   <div class="token-line">const y = 2;</div>
 * </code></pre></div>
 * ```
 *
 * Without handler:
 * ````md
 * ```
 * const x = 1;const y = 2;
 * ```
 * ````
 * With handler:
 * ````md
 * ```ts
 * const x = 1;
 * const y = 2;
 * ```
 * ````
 */
function docusaurusCodeBlockToMdast(codeBlock: hast.Element): mdast.Code {
  const codeLanguage = detectCodeLanguage(codeBlock);
  const codeText = hastSelect
    .selectAll(".token-line", codeBlock)
    .map((line) => hastTextContent(line))
    .join("\n")
    .replace(/\s+$/, "");
  return { type: "code", lang: codeLanguage || null, value: codeText };
}

/**
 * `language-text` means "no language", so we treat it as none.
 */
function detectCodeLanguage(codeBlock: hast.Element): string {
  for (const className of getClassNames(codeBlock)) {
    const match = className.match(/^language-(.+)$/);
    if (match && match[1] !== "text") {
      return match[1];
    }
  }
  return "";
}

/**
 * Renders an admonition back as the original MDX directive (`:::tip ... :::`),
 * via a remark-directive container node.
 *
 * @example
 * HTML:
 * ```html
 * <div class="theme-admonition theme-admonition-tip">
 *   <div class="admonitionHeading_...">tip</div>
 *   <div class="admonitionContent_..."><p>Be careful here.</p></div>
 * </div>
 * ```
 *
 * Without handler:
 * ```md
 * tip
 *
 * Be careful here.
 * ```
 * With handler:
 * ```md
 * :::tip
 *
 * Be careful here.
 *
 * :::
 * ```
 */
function docusaurusAdmonitionToMdast(
  state: hastToMdast.State,
  admonition: hast.Element,
): mdastDirective.ContainerDirective {
  const admonitionType = detectAdmonitionType(admonition);
  const admonitionCustomTitle = detectAdmonitionCustomTitle(admonition);
  const admonitionContent = hastSelect.select(
    '[class*="admonitionContent"]',
    admonition,
  );

  if (!admonitionContent) {
    throw Error("Empty admonition content.");
  }

  const children = state.all(
    admonitionContent,
  ) as mdastDirective.ContainerDirective["children"];

  const hasCustomTitle =
    admonitionCustomTitle !== "" &&
    admonitionCustomTitle.toLowerCase() !== admonitionType.toLowerCase();
  if (hasCustomTitle) {
    children.unshift({
      type: "paragraph",
      data: { directiveLabel: true },
      children: [{ type: "text", value: admonitionCustomTitle }],
    });
  }

  return {
    type: "containerDirective",
    name: admonitionType,
    children,
  };
}

function detectAdmonitionType(admonition: hast.Element): string {
  for (const className of getClassNames(admonition)) {
    const match = className.match(/^theme-admonition-(.+)$/);
    if (match) {
      return match[1];
    }
  }
  return "note";
}

/**
 * Returns the admonition's custom title (e.g. from `:::note[Gotcha]`) or an
 * empty string when it just uses the default type label.
 */
function detectAdmonitionCustomTitle(admonition: hast.Element): string {
  const heading = hastSelect.select('[class*="admonitionHeading"]', admonition);
  return heading ? hastTextContent(heading).trim() : "";
}

/**
 * Converts tabs to Markdown. All panels are present in the DOM, so we keep every
 * variant and label it with its tab title. The one exception is the
 * JavaScript/TypeScript code switcher: it shows the same snippet twice, so we
 * keep only the TypeScript variant.
 *
 * @example
 * HTML:
 * ```html
 * <div class="tabs-container">
 *   <ul role="tablist"><li role="tab">JavaScript</li><li role="tab">TypeScript</li></ul>
 *   <div><div role="tabpanel">...JS code...</div><div role="tabpanel">...TS code...</div></div>
 * </div>
 * ```
 *
 * Without handler:
 * ````md
 * -   JavaScript
 * -   TypeScript
 *
 * ```
 * const x = 1;
 * ```
 *
 * ```
 * const x: number = 1;
 * ```
 * ````
 * With handler:
 * ````md
 * ```ts
 * const x: number = 1;
 * ```
 * ````
 */
function docusaurusTabsToMdast(
  state: hastToMdast.State,
  tabs: hast.Element,
): mdast.RootContent[] {
  const tabsLabels = hastSelect
    .selectAll('[role="tab"]', tabs)
    .map((tab) => hastTextContent(tab).trim());
  const tabsPanels = hastSelect.selectAll('[role="tabpanel"]', tabs);

  if (tabsLabels.length !== tabsPanels.length) {
    throw Error("Tabs label count does not equal panel count.");
  }

  if (isJsTsTabsPair(tabsLabels)) {
    // For JS / TS tabs, we keep only the TypeScript version.
    const normalizedLabels = tabsLabels.map((label) => label.toLowerCase());
    const typescriptIndex = normalizedLabels.indexOf("typescript");
    return state.all(tabsPanels[typescriptIndex]);
  }

  return tabsPanels.flatMap((panel, index): mdast.RootContent[] => [
    {
      type: "paragraph",
      children: [
        {
          type: "strong",
          children: [{ type: "text", value: tabsLabels[index] }],
        },
      ],
    },
    ...state.all(panel),
  ]);
}

/**
 * Whether the tabs are exactly a JavaScript/TypeScript pair.
 */
function isJsTsTabsPair(tabsLabels: string[]): boolean {
  if (tabsLabels.length !== 2) {
    return false;
  }
  const normalizedLabels = tabsLabels.map((label) => label.toLowerCase());
  return (
    normalizedLabels.includes("javascript") &&
    normalizedLabels.includes("typescript")
  );
}

function hasClass(element: hast.Element, className: string): boolean {
  return getClassNames(element).includes(className);
}

function getClassNames(element: hast.Element): string[] {
  const className = element.properties?.className;
  return Array.isArray(className) ? className.map(String) : [];
}

function hastTextContent(node: hast.Nodes): string {
  if (node.type === "text") {
    return node.value;
  }
  if ("children" in node) {
    return node.children.map(hastTextContent).join("");
  }
  return "";
}
