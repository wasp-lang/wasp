import type * as hast from "hast";
import { Options as RehypeRemarkOptions } from "rehype-remark";
import { docusaurusAdmonitionToMdast } from "./handlers/docusaurus-admonition";
import { docusaurusCodeBlockToMdast } from "./handlers/docusaurus-code-block";
import { docusaurusTabsToMdast } from "./handlers/docusaurus-tabs";
import { hasClass } from "./hast-helpers";

export const docusaurusHtmlToMdHandlers: RehypeRemarkOptions = {
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
};
