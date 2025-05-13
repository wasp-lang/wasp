import type * as md from 'mdast'
import type { VFile } from 'vfile'

/**
 * Creates a "check fn" for `unist-util-visit` that
 * checks if a node is a code block with a specific meta flag.
 */
export function checkNodeIsCodeWithMeta(metaFlag: RegExp) {
  return (node: md.Nodes): node is md.Code & { meta: string } =>
    node.type === 'code' && node.meta && metaFlag.test(node.meta)
}

/**
 * Checks that the code block's language is supported.
 * If not, it throws an error with the block's position
 * (through VFile#fail)
 */
export function assertSupportedLanguage<T extends string>(
  node: md.Code,
  file: VFile,
  supportedLanguages: Set<T>
): asserts node is md.Code & { lang: T } {
  if (!node.lang) {
    file.fail(
      `No language specified. Please use one of: ${[...supportedLanguages].join(', ')}`,
      { place: node.position }
    )
  }

  if (!(supportedLanguages as Set<string>).has(node.lang)) {
    file.fail(
      `Unsupported language: ${node.lang}. Please use one of: ${[...supportedLanguages].join(', ')}`,
      { place: node.position }
    )
  }
}
