import { Options as RehypeRemarkOptions } from "rehype-remark";
import {
  DOCUSAURUS_ADMONITION_CLASS,
  docusaurusAdmonitionToMdast,
} from "./handlers/docusaurus-admonition";
import {
  DOCUSAURUS_CODE_BLOCK_CLASS,
  docusaurusCodeBlockToMdast,
} from "./handlers/docusaurus-code-block";
import {
  DOCUSAURUS_TABS_CLASS,
  docusaurusTabsToMdast,
} from "./handlers/docusaurus-tabs";
import { iframeToMdast, imageToMdast } from "./handlers/media";
import { hasClass } from "./hast-helpers";

export const docusaurusHtmlToMdHandlers: RehypeRemarkOptions = {
  handlers: {
    div(state, element) {
      if (hasClass(element, DOCUSAURUS_CODE_BLOCK_CLASS)) {
        return docusaurusCodeBlockToMdast(element);
      }
      if (hasClass(element, DOCUSAURUS_ADMONITION_CLASS)) {
        return docusaurusAdmonitionToMdast(state, element);
      }
      if (hasClass(element, DOCUSAURUS_TABS_CLASS)) {
        return docusaurusTabsToMdast(state, element);
      }
      return state.all(element);
    },
    iframe(_state, element) {
      return iframeToMdast(element);
    },
    img(_state, element) {
      return imageToMdast(element);
    },
  },
};
