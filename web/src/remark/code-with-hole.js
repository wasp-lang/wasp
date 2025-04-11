// @ts-check

/*
This file defines a plugin for the unified library that processes code blocks
in Markdown documents. It looks for code blocks with a specific meta flag
(`with-hole`) and replaces occurrences of the word "hole" in the code with
a placeholder comment (`...`). This is useful for code examples where
parts of the code are intentionally omitted for brevity or clarity, but
it still needs to be syntactically correct.
*/

const { visitParents } = require('unist-util-visit-parents')
const { default: escapeStringRegexp } = require('escape-string-regexp')

const ENABLED_META_FLAG = 'with-hole'
const HOLE_IDENTIFIER = 'hole'
const HOLE_REPLACEMENT = '/* ... */'

const SUPPORTED_LANGS = new Set(['js', 'jsx', 'ts', 'tsx'])

const wrapInWordBoundaries = (/** @type {string} */ reStr) => {
  return String.raw`\b${reStr}\b`
}

const enabledMetaRegexp = new RegExp(
  wrapInWordBoundaries(escapeStringRegexp(ENABLED_META_FLAG))
)

const holeIdentifierRegexp = new RegExp(
  wrapInWordBoundaries(escapeStringRegexp(HOLE_IDENTIFIER))
)

/** @type {import("unified").Plugin<[], import("mdast").Root>} */
const codeWithHolePlugin = () => {
  return (tree) => {
    visitParents(tree, 'code', (node) => {
      if (node.meta && enabledMetaRegexp.test(node.meta)) {
        if (!node.lang || !SUPPORTED_LANGS.has(node.lang))
          throw new Error(`Unsupported language: ${node.lang}`)

        // Remove our flag from the meta so other plugins don't trip up
        node.meta = node.meta.replace(enabledMetaRegexp, '')

        // Replace hole with ellipsis
        node.value = node.value.replace(holeIdentifierRegexp, HOLE_REPLACEMENT)
      }
    })
  }
}

module.exports = codeWithHolePlugin
