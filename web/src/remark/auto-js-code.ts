/*
From a TypeScript code block, generate a JavaScript version and wrop both in a
tabbed interface to switch between them.

Allows us to author code examples in TypeScript and have them automatically
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

// Type-only empty import to register MDX node types into mdast.
/// <reference types="mdast-util-mdx" />

import type * as md from 'mdast'
import * as path from 'node:path'
import { blankSourceFile } from 'ts-blank-space'
import * as ts from 'typescript'
import type { Plugin } from 'unified'
import { visit } from 'unist-util-visit'
import {
  assertSupportedLanguage,
  makeCheckForCodeWithMeta,
} from './util/code-blocks'
import { formatCode } from './util/prettier'

interface SupportedLanguageInfo {
  jsx: boolean
  outputLang: string
}

// Wrapped in \b to denote a word boundary.
const META_FLAG_REGEX = /\bauto-js\b/

const SUPPORTED_LANGUAGE_INFO = {
  ts: { jsx: false, outputLang: 'js' },
  tsx: { jsx: true, outputLang: 'jsx' },
} satisfies Record<string, SupportedLanguageInfo>

const SUPPORTED_EXTENSIONS = {
  '.ts': '.js',
  '.tsx': '.jsx',
}

const isCodeWithAutoJsFlag = makeCheckForCodeWithMeta(META_FLAG_REGEX)
const SUPPORTED_LANGUAGES = new Set(
  Object.keys(
    SUPPORTED_LANGUAGE_INFO
  ) as (keyof typeof SUPPORTED_LANGUAGE_INFO)[]
)

const autoJSCodePlugin: Plugin<[], md.Root> = () => async (tree, file) => {
  // `visit` does not allow async visitors, so we will just store the needed transformations
  // in an array and run them after the visit is done.
  const pendingCodeBlockTransforms: (() => Promise<void>)[] = []

  visit(tree, isCodeWithAutoJsFlag, (node, idx, parent) => {
    // Can't do async here, just store the transformation to run later.
    pendingCodeBlockTransforms.push(async () => {
      try {
        assertSupportedLanguage(node, SUPPORTED_LANGUAGES)
        const langInfo = SUPPORTED_LANGUAGE_INFO[node.lang]

        const jsCodeBlock = await makeJsCodeBlock(node, langInfo)
        const tsCodeBlock = await formatTsCodeBlock(node)

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
                {
                  type: 'mdxJsxAttribute',
                  name: 'label',
                  value: 'JavaScript',
                },
              ],
              children: [jsCodeBlock],
            },
            {
              type: 'mdxJsxFlowElement',
              name: 'TabItem',
              attributes: [
                { type: 'mdxJsxAttribute', name: 'value', value: 'ts' },
                {
                  type: 'mdxJsxAttribute',
                  name: 'label',
                  value: 'TypeScript',
                },
              ],
              children: [tsCodeBlock],
            },
          ],
        }

        // Replace input node for the new one in the parent's children array.
        parent.children.splice(idx, 1, newNode)
      } catch (err) {
        // We catch any thrown errors and annotate them as file errors, with the
        // code block position.
        file.fail(err, { place: node.position })
      }
    })
  })

  // We're out of the visitor, now we can run the transforms!
  for (const fn of pendingCodeBlockTransforms) {
    await fn()
  }
}

export default autoJSCodePlugin

// Taken from Docusaurus.
// https://github.com/facebook/docusaurus/blob/v2.4.3/packages/docusaurus-theme-common/src/utils/codeBlockUtils.ts
const CODE_BLOCK_TITLE_REGEX = /title=(?<quote>["'])(?<title>.*?)\1/

async function makeJsCodeBlock(
  originalNode: md.Code,
  langInfo: SupportedLanguageInfo
): Promise<md.Code> {
  // Find the `title=` meta param and change the extension.
  const newMeta = originalNode.meta?.replace(
    CODE_BLOCK_TITLE_REGEX,
    (_fullMatch, _quote, title) =>
      `title=${JSON.stringify(transformExt(title))}`
  )
  const lang = langInfo.outputLang
  const code = await formatCode(convertToJs(originalNode.value, langInfo), {
    parser: 'babel',
  })

  return {
    ...originalNode,
    value: code,
    lang: lang,
    meta: newMeta,
  }
}

// We format the original code block to make sure it is consistent with the other one.
async function formatTsCodeBlock(originalNode: md.Code): Promise<md.Code> {
  return {
    ...originalNode,
    value: await formatCode(originalNode.value, { parser: 'babel-ts' }),
  }
}

/*
  For the conversion to JS we use the `ts-blank-space` library, which is a
  TypeScript parser that can convert TS to JS -- by **only** removing the types.
  This is useful because we want to keep the code as close to the original as
  possible, and we don't run a full, slow TS compiler on it.

  We need to use `prettier` to format the code afterwards, because `ts-blank-space`
  leaves white space where types used to be, and we want to remove that.
*/
function convertToJs(tsCode: string, langInfo: SupportedLanguageInfo) {
  // We create a source file from ts so that way we can specify if we want to use
  // JSX or not, because the parsing is different. This is only done in memory.
  // Copied from the ts-blank-space playground.
  // https://github.com/bloomberg/ts-blank-space/blob/4102b1f26b1c53d38a1de74c10f262af5fd34fe8/website/play/play-utils.ts#L4
  const sourceFile = ts.createSourceFile(
    'input.ts',
    tsCode,
    { languageVersion: ts.ScriptTarget.ESNext },
    true,
    langInfo.jsx ? ts.ScriptKind.TSX : ts.ScriptKind.TS
  )

  return blankSourceFile(sourceFile)
}

function transformExt(inputPath: string) {
  const inputExt = path.extname(inputPath)
  const outputExt = SUPPORTED_EXTENSIONS[inputExt]

  if (!outputExt) {
    throw new Error(
      `Unsupported file extension: ${inputExt}. Please use one of: ${Object.keys(
        SUPPORTED_EXTENSIONS
      ).join(', ')}`
    )
  }

  const outputPath = inputPath.slice(0, -inputExt.length) + outputExt
  return outputPath
}
