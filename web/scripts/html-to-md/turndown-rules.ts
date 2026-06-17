import TurndownService from "turndown";
// turndown-plugin-gfm ships without types.
// @ts-expect-error -- no type declarations available
import { gfm } from "turndown-plugin-gfm";

/**
 * Turndown rules for Docusaurus output. Docusaurus renders MDX into HTML with
 * theme-specific wrappers (code blocks with copy buttons, admonitions, tabs).
 * Turndown's defaults mangle those, so we add rules that recognize the wrappers
 * and emit clean Markdown.
 */

type TurndownNode = HTMLElement;

const sharedOptions: TurndownService.Options = {
  headingStyle: "atx",
  codeBlockStyle: "fenced",
  fence: "```",
  bulletListMarker: "-",
  emDelimiter: "*",
  strongDelimiter: "**",
  linkStyle: "inlined",
};

/**
 * Creates the Turndown service used to convert doc HTML to Markdown. The base
 * service handles plain content; the returned full service adds rules for
 * admonitions and tabs that delegate back to the base service to convert their
 * inner content, which avoids infinite recursion.
 */
export function createTurndownService(): TurndownService {
  const base = buildBaseService();
  const full = buildBaseService();
  addAdmonitionRule(full, base);
  addTabsRule(full, base);
  return full;
}

function buildBaseService(): TurndownService {
  const td = new TurndownService(sharedOptions);
  td.use(gfm);
  td.remove(["script", "style", "button"]);
  addHashLinkRule(td);
  addCodeBlockRule(td);
  return td;
}

/** Drops heading anchor links (the "#" that appears on hover); they carry no content. */
function addHashLinkRule(td: TurndownService): void {
  td.addRule("hashLink", {
    filter: (node) =>
      node.nodeName === "A" && hasClass(node as TurndownNode, "hash-link"),
    replacement: () => "",
  });
}

/**
 * Converts a Docusaurus code block to a fenced block. Lines are separate
 * elements with no newline text nodes between them, so we join token-line
 * elements explicitly. The language lives on the container.
 *
 * @example
 * <div class="theme-code-block language-tsx ...">
 *   <div><pre><code><span class="token-line">...</span>...</code></pre></div>
 *   <div class="buttonGroup_..."><button>Copy</button></div>
 * </div>
 */
function addCodeBlockRule(td: TurndownService): void {
  td.addRule("docusaurusCodeBlock", {
    filter: (node) =>
      node.nodeName === "DIV" &&
      hasClass(node as TurndownNode, "theme-code-block"),
    replacement: (_content, node) => {
      const el = node as TurndownNode;
      const language = detectCodeLanguage(el);
      const code = extractCodeText(el);
      const fence = "```";
      return `\n\n${fence}${language}\n${code}\n${fence}\n\n`;
    },
  });
}

function detectCodeLanguage(node: TurndownNode): string {
  const fromContainer = findLanguageClass(node);
  if (fromContainer) {
    return fromContainer;
  }
  const pre = node.querySelector("pre");
  return pre ? findLanguageClass(pre) : "";
}

function findLanguageClass(el: Element | null): string {
  if (!el) {
    return "";
  }
  for (const className of Array.from(el.classList)) {
    const match = className.match(/^language-(.+)$/);
    if (match && match[1] !== "text") {
      return match[1];
    }
  }
  return "";
}

function extractCodeText(node: TurndownNode): string {
  const code = node.querySelector("code") ?? node.querySelector("pre");
  if (!code) {
    return (node.textContent ?? "").replace(/\s+$/, "");
  }
  const lines = code.querySelectorAll(".token-line");
  const text =
    lines.length > 0
      ? Array.from(lines)
          .map((line) => line.textContent ?? "")
          .join("\n")
      : (code.textContent ?? "");
  return text.replace(/\s+$/, "");
}

/**
 * Renders an admonition back as the original MDX directive (`:::tip ... :::`),
 * which is compact and round-trips to how the docs are authored.
 *
 * @example
 * <div class="theme-admonition theme-admonition-tip alert alert--success">
 *   <div class="admonitionHeading_..."><span class="...Icon">svg</span>tip</div>
 *   <div class="admonitionContent_...">...</div>
 * </div>
 */
function addAdmonitionRule(td: TurndownService, base: TurndownService): void {
  td.addRule("admonition", {
    filter: (node) =>
      node.nodeName === "DIV" &&
      hasClass(node as TurndownNode, "theme-admonition"),
    replacement: (_content, node) => {
      const el = node as TurndownNode;
      const type = detectAdmonitionType(el);
      const customTitle = detectAdmonitionCustomTitle(el, type);
      const contentEl = el.querySelector('[class*="admonitionContent"]') ?? el;
      const inner = base.turndown(contentEl.innerHTML).trim();
      const opening = customTitle ? `:::${type}[${customTitle}]` : `:::${type}`;
      return `\n\n${opening}\n\n${inner}\n\n:::\n\n`;
    },
  });
}

/**
 * Returns the admonition's custom title (e.g. from `:::note[Gotcha]`) or an
 * empty string when it just uses the default type label.
 */
function detectAdmonitionCustomTitle(node: TurndownNode, type: string): string {
  const heading = node.querySelector('[class*="admonitionHeading"]');
  const headingText = (heading?.textContent ?? "").trim();
  return headingText.toLowerCase() === type.toLowerCase() ? "" : headingText;
}

function detectAdmonitionType(node: TurndownNode): string {
  for (const className of Array.from(node.classList)) {
    const match = className.match(/^theme-admonition-(.+)$/);
    if (match) {
      return match[1];
    }
  }
  return "note";
}

/**
 * Converts tabs to Markdown. All panels are present in the DOM, so we keep every
 * variant and label it with its tab title. The one exception is the
 * JavaScript/TypeScript code switcher: it shows the same snippet twice, so we
 * keep only the TypeScript variant.
 *
 * @example
 * <div class="tabs-container">
 *   <ul role="tablist"><li role="tab">JavaScript</li>...</ul>
 *   <div><div role="tabpanel">...</div>...</div>
 * </div>
 */
function addTabsRule(td: TurndownService, base: TurndownService): void {
  td.addRule("tabs", {
    filter: (node) =>
      node.nodeName === "DIV" &&
      hasClass(node as TurndownNode, "tabs-container"),
    replacement: (_content, node) => {
      const el = node as TurndownNode;
      const labels = Array.from(el.querySelectorAll('[role="tab"]')).map(
        (tab) => (tab.textContent ?? "").trim(),
      );
      const panels = Array.from(el.querySelectorAll('[role="tabpanel"]'));

      const typescriptIndex = findTypescriptOnlyIndex(labels);
      if (typescriptIndex !== -1) {
        const panel = panels[typescriptIndex] ?? panels[panels.length - 1];
        return panel ? `\n\n${base.turndown(panel.innerHTML).trim()}\n\n` : "";
      }

      const parts = panels.map((panel, index) => {
        const label = labels[index] ?? `Tab ${index + 1}`;
        const inner = base.turndown(panel.innerHTML).trim();
        return `**${label}**\n\n${inner}`;
      });
      return `\n\n${parts.join("\n\n")}\n\n`;
    },
  });
}

/**
 * Returns the index of the TypeScript tab when the tabs are exactly a
 * JavaScript/TypeScript pair, otherwise -1.
 */
function findTypescriptOnlyIndex(labels: string[]): number {
  if (labels.length !== 2) {
    return -1;
  }
  const normalized = labels.map((label) => label.toLowerCase());
  const isJsTsPair =
    normalized.includes("javascript") && normalized.includes("typescript");
  return isJsTsPair ? normalized.indexOf("typescript") : -1;
}

function hasClass(node: TurndownNode, className: string): boolean {
  return node.classList?.contains(className) ?? false;
}
