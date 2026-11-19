import type * as hast from "hast";
import * as hastSelect from "hast-util-select";
import type * as hastToMdast from "hast-util-to-mdast";
import type * as mdast from "mdast";
import assert from "node:assert";
import { hastTextContent } from "../hast-helpers";

export const DOCUSAURUS_TABS_CLASS = "tabs-container";

/**
 * Converts Docusaurus tabs to markdown.
 * For JavaScript/TypeScript code switcher, we only keep the TypeScript variant.
 *
 * @example
 * Source HTML:
 * ```html
 * <div class="tabs-container">
 *   <ul role="tablist"><li role="tab">JavaScript</li><li role="tab">TypeScript</li></ul>
 *   <div><div role="tabpanel">...JS code...</div><div role="tabpanel">...TS code...</div></div>
 * </div>
 * ```
 *
 * Generated markdown without this function:
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
 * Generated markdown with this function:
 * ````md
 * ```ts
 * const x: number = 1;
 * ```
 * ````
 */
export function docusaurusTabsToMdast(
  state: hastToMdast.State,
  tabs: hast.Element,
): mdast.RootContent[] {
  const tabsLabels = selectOwnElements('[role="tab"]', tabs).map((tab) =>
    hastTextContent(tab).trim(),
  );
  const tabsPanels = selectOwnElements('[role="tabpanel"]', tabs);
  assert(
    tabsLabels.length === tabsPanels.length,
    "<Tabs> label count does not equal panel count.",
  );
  assert(
    tabsLabels.length >= 2,
    "<Tabs> should have at least 2 different tabs.",
  );

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
 * Selects the elements of these tabs, without the ones of tabs nested in a
 * panel. Those get converted when their panel does.
 */
function selectOwnElements(
  selector: string,
  tabs: hast.Element,
): hast.Element[] {
  const nestedTabs = hastSelect
    .selectAll(`.${DOCUSAURUS_TABS_CLASS}`, tabs)
    .filter((element) => element !== tabs);
  const nestedElements = new Set(
    nestedTabs.flatMap((nested) => hastSelect.selectAll(selector, nested)),
  );
  return hastSelect
    .selectAll(selector, tabs)
    .filter((element) => !nestedElements.has(element));
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
