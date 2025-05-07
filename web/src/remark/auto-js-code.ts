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

import type * as md from 'mdast'
import type { } from 'mdast-util-mdx'; // Type-only empty import to register MDX types into mdast
import assert from 'node:assert/strict'
import * as path from 'node:path'
import * as prettier from 'prettier'
import { blankSourceFile } from 'ts-blank-space'
import * as ts from 'typescript'
import type { Plugin } from 'unified'
import { visitParents } from 'unist-util-visit-parents'

// Wrapped in \b to denote a word boundary
const META_FLAG_REGEX = /\bauto-js\b/
const SUPPORTED_LANGS = new Set(['ts', 'tsx'])

const autoJSCodePlugin: Plugin<[], md.Root> = () => async (tree, file) => {
  const nodesToProcess = new Set<{ node: md.Code; ancestors: md.Parents[] }>()

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

    const jsCodeBlock = await makeJsCodeBlock(newMeta, node, {
      location: file.path,
    })
    const tsCodeBlock = await makeTsCodeBlock(newMeta, node, {
      location: file.path,
    })

    // The specific structure of the new node was retrieved by copy-pasting
    // an example into the MDX playground and inspecting the AST.
    // https://mdxjs.com/playground
    const newNode: md.RootContent = {
      type: 'mdxJsxFlowElement',
      name: 'Tabs',
      attributes: [
        { type: 'mdxJsxAttribute', name: 'groupId', value: 'js-ts' },
      ],
      children: [
        {
          type: 'mdxJsxFlowElement',
          name: 'TabItem',
          attributes: [
            { type: 'mdxJsxAttribute', name: 'value', value: 'js' },
            { type: 'mdxJsxAttribute', name: 'label', value: 'JavaScript' },
          ],
          children: [jsCodeBlock],
        },
        {
          type: 'mdxJsxFlowElement',
          name: 'TabItem',
          attributes: [
            { type: 'mdxJsxAttribute', name: 'value', value: 'ts' },
            { type: 'mdxJsxAttribute', name: 'label', value: 'TypeScript' },
          ],
          children: [tsCodeBlock],
        },
      ],
    }

    const idx = parent.children.findIndex((someNode) => someNode === node)
    assert(idx !== -1, "Node not found in parent's children")

    // Replace input node for the new ones in the parent's children array
    parent.children.splice(idx, 1, newNode)
  }
}

export default autoJSCodePlugin

// Taken from Docusaurus
// https://github.com/facebook/docusaurus/blob/v2.4.3/packages/docusaurus-theme-common/src/utils/codeBlockUtils.ts
const CODE_BLOCK_TITLE_REGEX = /title=(?<quote>["'])(?<title>.*?)\1/

async function makeJsCodeBlock(
  metaString: string,
  node: md.Code,
  { location }: { location: string }
): Promise<md.Code> {
  // Find the `title=` meta param and change the extension
  const meta = metaString.replace(
    CODE_BLOCK_TITLE_REGEX,
    (_fullMatch, _quote, title) =>
      `title=${JSON.stringify(
        transformExt(title, (ext) => ext.replace('ts', 'js'))
      )}`
  )
  const lang = node.lang?.replace('ts', 'js')
  const isJsx = node.lang.endsWith('x')
  const code = await format(convertToJs(node.value, { jsx: isJsx }), {
    parser: 'babel',
    location,
  })

  return {
    type: 'code',
    value: code,
    lang: lang,
    meta: meta,
  }
}

async function makeTsCodeBlock(
  metaString: string,
  node: md.Code,
  { location }: { location: string }
): Promise<md.Code> {
  const lang = node.lang
  const code = await format(node.value, { parser: 'babel-ts', location })

  return {
    type: 'code',
    value: code,
    lang: lang,
    meta: metaString,
  }
}

function convertToJs(code: string, { jsx }: { jsx: boolean }) {
  // We create a source file from ts so that way we can specify if
  // we want to use JSX or not, because the parsing is different.
  // Copied from the ts-blank-space playground
  // https://github.com/bloomberg/ts-blank-space/blob/4102b1f26b1c53d38a1de74c10f262af5fd34fe8/website/play/play-utils.ts#L4
  const sourceFile = ts.createSourceFile(
    'input.ts',
    code,
    { languageVersion: ts.ScriptTarget.ESNext },
    true,
    jsx ? ts.ScriptKind.TSX : ts.ScriptKind.TS
  )

  return blankSourceFile(sourceFile)
}

async function format(
  code: string,
  { parser, location }: { parser: prettier.Options['parser']; location: string }
) {
  const config = await prettier.resolveConfig(location, {
    useCache: true,
    editorconfig: true,
  })

  return await prettier.format(code, { ...config, parser })
}

function transformExt(inPath: string, fn: (ext: string) => string) {
  const inExt = path.extname(inPath)
  const outExt = fn(inExt)
  const outPath = inPath.slice(0, -inExt.length) + outExt
  return outPath
}
