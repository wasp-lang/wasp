/*
This file defines a plugin for the unified library that processes code blocks
in Markdown documents. It looks for code blocks with a specific meta flag
(`with-hole`) and replaces occurrences of the word "hole" in the code with
a placeholder comment (`...`). This is useful for code examples where
parts of the code are intentionally omitted for brevity or clarity, but
it still needs to be syntactically correct.

Example:

Input:

    ```js title="src/apis.js" with-hole
    export const validatePassword = (password) => password.length > 8 && hole;
    ```

Output:

    ```js title="src/apis.js"
    export const validatePassword = (password) => password.length > 8 && ...;
    ```

*/

import { Root } from 'mdast'
import { Plugin } from 'unified'
import { visitParents } from 'unist-util-visit-parents'

// Wrapped in \b to denote a word boundary
const META_FLAG_REGEX = /\bwith-hole\b/
const HOLE_IDENTIFIER_REGEX = /\bhole\b/
const HOLE_REPLACEMENT = '/* ... */'

const SUPPORTED_LANGS = new Set(['js', 'jsx', 'ts', 'tsx'])

const codeWithHolePlugin: Plugin<[], Root> = () => {
  return (tree) => {
    visitParents(tree, 'code', (node) => {
      if (node.meta && META_FLAG_REGEX.test(node.meta)) {
        if (!node.lang || !SUPPORTED_LANGS.has(node.lang)) {
          throw new Error(`Unsupported language: ${node.lang}`)
        }

        // Remove our flag from the meta so other plugins don't trip up
        node.meta = node.meta.replace(META_FLAG_REGEX, '')

        // Replace hole with ellipsis
        node.value = node.value.replace(HOLE_IDENTIFIER_REGEX, HOLE_REPLACEMENT)
      }
    })
  }
}

export default codeWithHolePlugin
