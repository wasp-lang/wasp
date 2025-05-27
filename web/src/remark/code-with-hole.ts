/*
In a code block tagged with `with-hole` meta, replace the word "$HOLE$" with a comment
`/ * ... * /`. (Please note that the spaces in the comment are intentional to avoid
the comment being parsed as a code block.)

This is useful for keeping code examples syntactically correct (for example for use
with `auto-js`) while
omitting parts of the code for clarity or brevity.

Example:

Input:

    ```js title="src/apis.js" with-hole
    export const validatePassword = (password) => password.length > 8 && $HOLE$;
    ```

Output:

    ```js title="src/apis.js"
    export const validatePassword = (password) => password.length > 8 && ...;
    ```

*/

import type * as md from "mdast";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import {
  assertSupportedLanguage,
  makeCheckForCodeWithMeta,
} from "./util/code-blocks";

const META_FLAG = "with-hole";
const HOLE_IDENTIFIER_REGEX = /\b\$HOLE$\b/; // Wrapped in \b to denote a word boundary.
const HOLE_REPLACEMENT = "/* ... */";

const SUPPORTED_LANGS = ["js", "jsx", "ts", "tsx"] as const;

const isCodeWithHoleFlag = makeCheckForCodeWithMeta(META_FLAG);

const codeWithHolePlugin: Plugin<[], md.Root> = () => (tree, file) => {
  visit(tree, isCodeWithHoleFlag, (node) => {
    try {
      assertSupportedLanguage(node, SUPPORTED_LANGS);

      // Replace hole with ellipsis.
      node.value = node.value.replace(HOLE_IDENTIFIER_REGEX, HOLE_REPLACEMENT);
    } catch (error) {
      // We catch any thrown errors and annotate them as file errors, with the
      // code block position.
      file.fail(error, { place: node.position });
    }
  });
};

export default codeWithHolePlugin;
