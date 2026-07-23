import type * as hast from "hast";
import * as hastSelect from "hast-util-select";
import type * as hastToMdast from "hast-util-to-mdast";
import type * as mdastDirective from "mdast-util-directive";
import { getClassNames, hastTextContent } from "../hast-helpers";

/**
 * Renders an admonition back as the original MDX directive (`:::tip ... :::`),
 * via a remark-directive container node.
 *
 * @example
 * Target HTML:
 * ```html
 * <div class="theme-admonition theme-admonition-tip">
 *   <div class="admonitionHeading_...">tip</div>
 *   <div class="admonitionContent_..."><p>Be careful here.</p></div>
 * </div>
 * ```
 *
 * Generated markdown without this function:
 * ```md
 * tip
 *
 * Be careful here.
 * ```
 * Generated markdown with this function:
 * ```md
 * :::tip
 * Be careful here.
 * :::
 * ```
 */
export function docusaurusAdmonitionToMdast(
  state: hastToMdast.State,
  admonition: hast.Element,
): mdastDirective.ContainerDirective {
  const admonitionType = detectAdmonitionType(admonition);
  const admonitionCustomTitle = detectAdmonitionCustomTitle(admonition);
  const admonitionContentEl = hastSelect.select(
    '[class*="admonitionContent"]',
    admonition,
  );

  if (!admonitionContentEl) {
    throw Error("Empty admonition content.");
  }

  const children = state.all(
    admonitionContentEl,
  ) as mdastDirective.ContainerDirective["children"];

  const hasCustomTitle =
    !!admonitionCustomTitle &&
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
function detectAdmonitionCustomTitle(admonition: hast.Element): string | null {
  const heading = hastSelect.select('[class*="admonitionHeading"]', admonition);
  return heading ? hastTextContent(heading).trim() : null;
}
