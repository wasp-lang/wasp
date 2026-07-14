import type * as hast from "hast";
import * as hastSelect from "hast-util-select";
import type * as mdast from "mdast";
import { getClassNames, hastTextContent } from "../hast-helpers";

/**
 * Converts a Docusaurus code block to a Markdown code block.
 *
 * @example
 * Target HTML:
 * ```html
 * <div class="theme-code-block language-ts">
 *   <div class="codeBlockTitle">main.wasp.ts</div>
 *   <pre><code>
 *     <div class="token-line">const x = 1;</div>
 *     <div class="token-line">const y = 2;</div>
 *   </code></pre>
 * </div>
 * ```
 *
 * Generated markdown without this function:
 * ````md
 * main.wasp.ts
 *
 * ```
 * const x = 1;const y = 2;
 * ```
 * ````
 * Generated markdown with this function:
 * ````md
 * ```ts title="main.wasp.ts"
 * const x = 1;
 * const y = 2;
 * ```
 * ````
 */
export function docusaurusCodeBlockToMdast(
  codeBlock: hast.Element,
): mdast.Code {
  const codeLanguage = detectCodeBlockLanguage(codeBlock);
  const codeTitle = detectCodeBlockTitle(codeBlock);
  const codeText = hastSelect
    .selectAll(".token-line", codeBlock)
    .map((line) => hastTextContent(line))
    .join("\n")
    .replace(/\s+$/, "");

  if (!codeText) {
    throw new Error("Empty or missing code block content.");
  }

  return {
    type: "code",
    lang: codeLanguage ? codeLanguage : undefined,
    meta: codeTitle ? `title="${codeTitle}"` : undefined,
    value: codeText,
  };
}

function detectCodeBlockTitle(codeBlock: hast.Element): string | null {
  const titleEl = hastSelect.select('[class*="codeBlockTitle"]', codeBlock);
  return titleEl ? hastTextContent(titleEl).trim() : null;
}

/**
 * `language-text` means "no language", so we treat it as none.
 */
function detectCodeBlockLanguage(codeBlock: hast.Element): string | null {
  for (const className of getClassNames(codeBlock)) {
    const match = className.match(/^language-(.+)$/);
    if (match && match[1] !== "text") {
      return match[1];
    }
  }
  return null;
}
