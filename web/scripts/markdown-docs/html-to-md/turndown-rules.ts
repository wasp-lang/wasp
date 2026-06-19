import { gfm as githubFlavoredMarkdown } from "@joplin/turndown-plugin-gfm";
import TurndownService from "turndown";

/**
 * Turndown rules for Docusaurus HTML output.
 * Docusaurus renders MDX into HTML with theme-specific wrappers.
 * Turndown's defaults mangle those, so we add rules that recognize the wrappers
 * and emit clean Markdown.
 */

/**
 * Creates the Turndown service used to convert Docusaurus doc HTML to Markdown.
 */
export function createDocusaurusLlmFriendlyTurndownService(): TurndownService {
  const turndownService = new TurndownService({
    headingStyle: "atx",
    codeBlockStyle: "fenced",
    fence: "```",
    bulletListMarker: "-",
    emDelimiter: "*",
    strongDelimiter: "**",
    linkStyle: "inlined",
  });
  turndownService.use(githubFlavoredMarkdown);
  removeDocusaurusSecretGeneratorButton(turndownService);
  addDocusaurusHashLinkRule(turndownService);
  addDocusaurusCodeBlockRule(turndownService);
  addDocusaurusAdmonitionRule(turndownService);
  addDocusaurusTabsRule(turndownService);

  return turndownService;
}

/**
 * Drops the "Generate secret" widget button.
 * Without this its label leaks into the Markdown as prose.
 *
 * @example
 * HTML:
 * ```html
 * <td>A random string. <button class="generateBtn_CFbx">Generate secret</button></td>
 * ```
 *
 * Without rule:
 * ```md
 * A random string. Generate secret
 * ```
 * With rule:
 * ```md
 * A random string.
 * ```
 */
function removeDocusaurusSecretGeneratorButton(td: TurndownService): void {
  td.remove(
    (node) =>
      node.nodeName === "BUTTON" &&
      Array.from(node.classList).some((className) =>
        className.startsWith("generateBtn"),
      ),
  );
}

/**
 * Drops heading anchor links (the "#" that appears on hover).
 * They carry no content.
 *
 * @example
 * HTML:
 * ```html
 * <h2>Title<a class="hash-link" href="#title"></a></h2>
 * ```
 *
 * Without rule:
 * ```md
 * ## Title[](#title)
 * ```
 * With rule:
 * ```md
 * ## Title
 * ```
 */
function addDocusaurusHashLinkRule(td: TurndownService): void {
  td.addRule("docusaurusHashLink", {
    filter: (node) => node.nodeName === "A" && hasClass(node, "hash-link"),
    replacement: () => "",
  });
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
 * Without rule:
 * ````md
 * ```
 * const x = 1;const y = 2;
 * ```
 * ````
 * With rule:
 * ````md
 * ```ts
 * const x = 1;
 * const y = 2;
 * ```
 * ````
 */
function addDocusaurusCodeBlockRule(td: TurndownService): void {
  td.addRule("docusaurusCodeBlock", {
    filter: (node) =>
      node.nodeName === "DIV" && hasClass(node, "theme-code-block"),
    replacement: (_content, node) => {
      const codeLanguage = detectCodeLanguage(node);
      const codeText = extractCodeText(node);
      const fence = "```";
      return `\n\n${fence}${codeLanguage}\n${codeText}\n${fence}\n\n`;
    },
  });
}

/**
 * `language-text` means "no language", so we treat it as none.
 */
function detectCodeLanguage(node: HTMLElement): string {
  for (const className of Array.from(node.classList)) {
    const match = className.match(/^language-(.+)$/);
    if (match && match[1] !== "text") {
      return match[1];
    }
  }
  return "";
}

function extractCodeText(node: HTMLElement): string {
  const code = node.querySelector("code") ?? node.querySelector("pre");
  if (!code) {
    throw Error("Code block has no <code> or <pre> element.");
  }
  return Array.from(code.querySelectorAll(".token-line"))
    .map((line) => line.textContent ?? "")
    .join("\n")
    .replace(/\s+$/, "");
}

/**
 * Renders an admonition back as the original MDX directive (`:::tip ... :::`).
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
 * Without rule:
 * ```md
 * tip
 *
 * Be careful here.
 *
 * ```
 * With rule:
 * ```md
 * :::tip
 *
 * Be careful here.
 *
 * :::
 * ```
 */
function addDocusaurusAdmonitionRule(td: TurndownService): void {
  td.addRule("docusaurusAdmonition", {
    filter: (node) =>
      node.nodeName === "DIV" && hasClass(node, "theme-admonition"),
    replacement: (_content, node) => {
      const admonitionType = detectAdmonitionType(node);
      const customTitle = detectAdmonitionCustomTitle(node);
      const admonitionContent = node.querySelector(
        '[class*="admonitionContent"]',
      );

      if (!admonitionContent) {
        throw Error("Empty admonition content.");
      }

      const admonitionMarkdownContent = td
        .turndown(admonitionContent.innerHTML)
        .trim();

      const opening =
        admonitionType.toLowerCase() === customTitle.toLowerCase()
          ? `:::${admonitionType}`
          : `:::${admonitionType}[${customTitle}]`;
      return `\n\n${opening}\n\n${admonitionMarkdownContent}\n\n:::\n\n`;
    },
  });
}

function detectAdmonitionType(node: HTMLElement): string {
  for (const className of Array.from(node.classList)) {
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
function detectAdmonitionCustomTitle(node: HTMLElement): string {
  const heading = node.querySelector('[class*="admonitionHeading"]');
  return (heading?.textContent ?? "").trim();
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
 * Without rule:
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
 * With rule:
 * ````md
 * ```ts
 * const x: number = 1;
 * ```
 * ````
 */
function addDocusaurusTabsRule(td: TurndownService): void {
  td.addRule("docusaurusTabs", {
    filter: (node) =>
      node.nodeName === "DIV" && hasClass(node, "tabs-container"),
    replacement: (_content, node) => {
      const tabsLabels = Array.from(node.querySelectorAll('[role="tab"]')).map(
        (tab) => (tab.textContent ?? "").trim(),
      );
      const tabsPanels = Array.from(node.querySelectorAll('[role="tabpanel"]'));

      if (tabsLabels.length !== tabsPanels.length) {
        throw Error("Tabs label count does not equal panel count.");
      }

      if (isJsTsTabsPair(tabsLabels)) {
        // For JS / TS tabs, we return only the TypeScript verison.
        const normalizedLabels = tabsLabels.map((label) => label.toLowerCase());
        const typescriptIndex = normalizedLabels.indexOf("typescript");
        const typescriptTabsPanel = tabsPanels[typescriptIndex];
        return `\n\n${td.turndown(typescriptTabsPanel.innerHTML).trim()}\n\n`;
      }

      const markdownTabs = tabsPanels.map((panel, index) => {
        const label = tabsLabels[index];
        const markdownPanel = td.turndown(panel.innerHTML).trim();
        return `**${label}**\n\n${markdownPanel}`;
      });
      return `\n\n${markdownTabs.join("\n\n")}\n\n`;
    },
  });
}

/**
 * Returns the index of the TypeScript tab when the tabs are exactly a
 * JavaScript/TypeScript pair, otherwise `null`.
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

function hasClass(htmlElement: HTMLElement, className: string): boolean {
  return htmlElement.classList?.contains(className) ?? false;
}
