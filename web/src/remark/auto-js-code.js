// @ts-check

/*
This file defines a plugin for the unified library that processes code blocks
in Markdown documents. It looks for code blocks with a specific meta flag
(`auto-js`) and replaces them with a pair of code blocks: one for JavaScript
and one for TypeScript, as well as a tabbed interface to switch between them.
This way we can author code examples in TypeScript and have them automatically
converted to JavaScript for the documentation.

Example:

Input:

    ```ts title="src/apis.ts" auto-js
    export const validatePassword = (password: string) => password.length > 8;
    ```

Output:

    <Tabs groupId="js-ts">
    <TabItem value="js" label="JavaScript">

    ```js title="src/apis.js"
    export const validatePassword = (password) => password.length > 8;
    ```

    </TabItem>
    <TabItem value="ts" label="TypeScript">

    ```ts title="src/apis.ts"
    export const validatePassword = (password: string) => password.length > 8;
    ```

    </TabItem>
    </Tabs>

*/

const assert = require('assert/strict')
const { visitParents } = require('unist-util-visit-parents')
const { default: tsBlankSpace } = require('ts-blank-space')
const prettier = require('prettier')
const path = require('path')

// Wrapped in \b to denote a word boundary
const META_FLAG_REGEX = /\bauto-js\b/

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

const format = async (
  /** @type {string} */ code,
  /** @type {{ parser: prettier.Options["parser"], location: string }} */ {
    parser,
    location,
  }
) => {
  const config = await prettier.resolveConfig(location, {
    useCache: true,
    editorconfig: true,
  })

  return await prettier.format(code, { ...config, parser })
}

/** @type {import("unified").Plugin<[], import("mdast").Root>} */
const autoJSCodePlugin = () => {
  return async (tree, file) => {
    /** @type {Set<{ node: import("mdast").Code, ancestors: import("mdast").Parents[] }>} */
    const nodesToProcess = new Set()

    visitParents(tree, 'code', (node, ancestors) => {
      if (node.meta && META_FLAG_REGEX.test(node.meta)) {
        if (!node.lang || !SUPPORTED_LANGS.has(node.lang)) {
          throw new Error(`Unsupported language: ${node.lang}`)
        }

        // We put these aside for processing later
        // because `visitParents` does not allow
        // async visitors.
        nodesToProcess.add({ node, ancestors })
      }
    })

    for (const { node, ancestors } of nodesToProcess) {
      const parent = ancestors.at(-1)
      assert(parent) // It must have a parent because the root node is a fully formed tree
      assert(node.meta && node.lang) // Already checked in the visitor

      // Remove our flag from the meta so other plugins don't trip up
      const newMeta = node.meta.replace(META_FLAG_REGEX, '')

      const tsMeta = newMeta
      const tsLang = node.lang
      const tsCode = await format(node.value, {
        parser: 'babel-ts',
        location: file.path,
      })

      // Find the `title=` meta param and change the extension
      // from ts to js
      const jsMeta = newMeta.replace(
        codeBlockTitleRegex,
        (_fullMatch, _quote, title) =>
          `title=${JSON.stringify(
            transformExt(title, (ext) => ext.replace('ts', 'js'))
          )}`
      )
      const jsLang = node.lang.replace('ts', 'js')
      const jsCode = await format(tsBlankSpace(node.value), {
        parser: 'babel',
        location: file.path,
      })

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
      assert(idx !== -1, "Node not found in parent's children")

      // Replace input node for the new ones in the parent's children array
      parent.children.splice(idx, 1, ...newNodes)
    }
  }
}

module.exports = autoJSCodePlugin
