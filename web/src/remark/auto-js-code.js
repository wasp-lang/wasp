// @ts-check

const assert = require('assert/strict')
const { visitParents } = require('unist-util-visit-parents')
const { default: tsBlankSpace } = require('ts-blank-space')
const { default: escapeStringRegexp } = require('escape-string-regexp')
const { format } = require('prettier')
const path = require('path')

const ENABLED_META_FLAG = 'auto-js'

const SUPPORTED_LANGS = new Set(['ts', 'tsx'])

// Taken from Docusaurus
// https://github.com/facebook/docusaurus/blob/v2.4.3/packages/docusaurus-theme-common/src/utils/codeBlockUtils.ts
const codeBlockTitleRegex = /title=(?<quote>["'])(?<title>.*?)\1/

const transformExt = (
  /** @type {string} */ inPath,
  /** @type {(ext: string) => string} */ fn
) => {
  const inExt = path.extname(inPath)
  const outExt = fn(inExt)
  const outPath = inPath.slice(0, -inExt.length) + outExt
  return outPath
}

/** @type {import("unified").Plugin<[], import("mdast").Root>} */
const autoJSCodePlugin = () => {
  const enabledMetaRegexp = new RegExp(
    String.raw`\b${escapeStringRegexp(ENABLED_META_FLAG)}\b`
  )

  return async (tree) => {
    /** @type {Set<{ node: import("mdast").Code, ancestors: import("mdast").Parents[] }>} */
    const nodesToProcess = new Set()

    visitParents(tree, 'code', (node, ancestors) => {
      if (node.meta && enabledMetaRegexp.test(node.meta)) {
        if (!node.lang || !SUPPORTED_LANGS.has(node.lang))
          throw new Error(`Unsupported language: ${node.lang}`)

        // We put these aside for processing later
        // because `visitParents` does not allow
        // async visitors.
        nodesToProcess.add({ node, ancestors })
      }
    })

    for (const { node, ancestors } of nodesToProcess) {
      const parent = ancestors.at(-1)
      assert(parent)
      assert(node.meta)

      // Remove our flag from the meta so other plugins don't trip up
      const newMeta = node.meta.replace(enabledMetaRegexp, '')

      const tsMeta = newMeta
      const tsLang = node.lang
      const tsCode = await format(node.value, { parser: 'babel-ts' })

      // Find the `title=` meta param and change the extension
      // from ts to js
      const jsMeta = newMeta.replace(
        codeBlockTitleRegex,
        (_fullMatch, _quote, title) =>
          `title=${JSON.stringify(
            transformExt(title, (ext) => ext.replace('ts', 'js'))
          )}`
      )
      const jsLang = node.lang?.replace('ts', 'js')
      const jsCode = await format(tsBlankSpace(node.value), { parser: 'babel' })

      /** @type {import("mdast").RootContent[]} */
      const newNodes = [
        {
          // @ts-expect-error This is an MDX extension
          type: 'jsx',
          value: `<Tabs groupId="js-ts"><TabItem value="js" label="JavaScript">`,
        },
        {
          type: 'code',
          value: jsCode,
          lang: jsLang,
          meta: jsMeta,
        },
        {
          // @ts-expect-error This is an MDX extension
          type: 'jsx',
          value: `</TabItem><TabItem value="ts" label="TypeScript">`,
        },
        {
          type: 'code',
          value: tsCode,
          lang: tsLang,
          meta: tsMeta,
        },
        {
          // @ts-expect-error This is an MDX extension
          type: 'jsx',
          value: `</TabItem></Tabs>`,
        },
      ]

      const idx = parent.children.findIndex((someNode) => someNode === node)
      assert(idx !== -1)

      // Replace input node for the new ones in the parent's children array
      parent.children.splice(idx, 1, ...newNodes)
    }
  }
}

module.exports = autoJSCodePlugin
