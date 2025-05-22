import type * as md from 'mdast'

/**
 * Creates a "check fn" for `unist-util-visit` that checks if a node is a code
 * block with a specific meta flag.
 */
export function makeCheckForCodeWithMeta(metaFlag: RegExp) {
  return (node: md.Nodes): node is md.Code & { meta: string } =>
    node.type === 'code' && node.meta && metaFlag.test(node.meta)
}

/**
 * Checks that the code block's language is supported. If not, it throws an error.
 */
export function assertSupportedLanguage<
  Language extends string,
  CodeNode extends md.Code,
>(
  node: CodeNode,
  supportedLanguages: Set<Language>
): asserts node is CodeNode & { lang: Language } {
  let errorMessage: string | undefined

  if (!node.lang) {
    errorMessage = 'No language specified.'
  } else if (!(supportedLanguages as Set<string>).has(node.lang)) {
    errorMessage = `Unsupported language: ${node.lang}.`
  }

  if (errorMessage) {
    const solutionMessage = `Please use one of: ${[...supportedLanguages].join(', ')}`
    throw new Error([errorMessage, solutionMessage].join('\n'))
  }
}
