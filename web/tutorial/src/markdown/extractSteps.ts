import fs from 'fs/promises'
import path from 'path'

import * as acorn from 'acorn'
import { mdxJsx } from 'micromark-extension-mdx-jsx'
import { fromMarkdown } from 'mdast-util-from-markdown'
import { mdxJsxFromMarkdown, type MdxJsxFlowElement } from 'mdast-util-mdx-jsx'
import { visit } from 'unist-util-visit'

import { type Action, createApplyPatchAction } from '../actions/index'
import searchAndReplace from '../../../src/remark/search-and-replace.js'

const componentName = 'TutorialAction'

export async function getActionsFromTutorialFiles(): Promise<Action[]> {
  const files = await fs
    .readdir(path.resolve('../docs/tutorial'))
    .then((files) =>
      files
        .filter((file) => file.endsWith('.md'))
        .sort((a, b) => {
          const aNumber = parseInt(a.split('-')[0]!, 10)
          const bNumber = parseInt(b.split('-')[0]!, 10)
          return aNumber - bNumber
        })
    )
  const actions: Action[] = []
  for (const file of files) {
    console.log(`Processing file: ${file}`)
    const fileActions = await getActionsFromFile(
      path.resolve('../docs/tutorial', file)
    )
    actions.push(...fileActions)
  }
  return actions
}

async function getActionsFromFile(filePath: string): Promise<Action[]> {
  const actions = [] as Action[]
  const doc = await fs.readFile(path.resolve(filePath))

  const ast = fromMarkdown(doc, {
    extensions: [mdxJsx({ acorn, addResult: true })],
    mdastExtensions: [mdxJsxFromMarkdown()],
  })

  // TODO: figure this out
  // @ts-ignore
  searchAndReplace.visitor(ast)

  visit(ast, 'mdxJsxFlowElement', (node) => {
    if (node.name !== componentName) {
      return
    }
    const step = getStep(node)
    const action = getAttributeValue(node, 'action')

    if (!step || !action) {
      throw new Error('Step and action attributes are required')
    }

    if (action === 'migrate-db') {
      actions.push({
        kind: 'migrate-db',
        step,
      })
      return
    }

    if (node.children.length !== 1) {
      throw new Error(`${componentName} must have exactly one child`)
    }

    const childCode = node.children[0]
    if (childCode === undefined || childCode.type !== 'code') {
      throw new Error(`${componentName} must have a code child`)
    }

    const codeBlockCode = childCode.value

    if (action === 'diff') {
      actions.push(createApplyPatchAction(codeBlockCode, step))
    } else if (action === 'write') {
      const path = getAttributeValue(node, 'path')
      if (!path) {
        throw new Error('Path attribute is required for write action')
      }
      actions.push({
        kind: 'write',
        content: codeBlockCode,
        path,
        step,
      })
    }
  })

  return actions
}

function getStep(node: MdxJsxFlowElement): number | null {
  const step = getAttributeValue(node, 'step')
  return step !== null ? parseInt(step, 10) : null
}

function getAttributeValue(
  node: MdxJsxFlowElement,
  attributeName: string
): string | null {
  const attribute = node.attributes.find(
    (attr) => attr.type === 'mdxJsxAttribute' && attr.name === attributeName
  )
  return attribute && typeof attribute.value === 'string'
    ? attribute.value
    : null
}
