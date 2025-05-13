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

import type * as md from 'mdast'
import type { Plugin } from 'unified'
import { visit } from 'unist-util-visit'
import { assertSupportedLanguage, checkNodeIsCodeWithMeta } from './util/code-blocks'

// Wrapped in \b to denote a word boundary
const META_FLAG_REGEX = /\bwith-hole\b/
const HOLE_IDENTIFIER_REGEX = /\bhole\b/
const HOLE_REPLACEMENT = '/* ... */'

const SUPPORTED_LANGS = new Set(['js', 'jsx', 'ts', 'tsx'] as const)

const codeWithHolePlugin: Plugin<[], md.Root> = () => (tree, file) => {
  visit(tree, checkNodeIsCodeWithMeta(META_FLAG_REGEX), (node) => {
    assertSupportedLanguage(node, file, SUPPORTED_LANGS)

    // Remove our flag from the meta so other plugins don't trip up
    node.meta = node.meta.replace(META_FLAG_REGEX, '')

    // Replace hole with ellipsis
    node.value = node.value.replace(HOLE_IDENTIFIER_REGEX, HOLE_REPLACEMENT)
  })
}

export default codeWithHolePlugin
