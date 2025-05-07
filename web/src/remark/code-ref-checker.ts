import fs from 'fs'
import type { Code, Root } from 'mdast'
import path from 'path'
import type { Plugin } from 'unified'
import { SKIP, visit } from 'unist-util-visit'
import url from 'url'

// TODO: Add explanation of what this plugin is about. Examples.

const plugin: Plugin<[], Root> = () => {
  return (tree, _file) => {
    visit(tree, 'code', (codeBlockNode) => {
      const codeRefString = obtainCodeRefStringFromCodeBlock(codeBlockNode)
      if (!codeRefString) return SKIP

      const codeRef = parseCodeRefString(codeRefString)
      console.log(codeRef)

      const docCodeBlock = codeBlockNode.value
      const refCodeBlock = fetchCodeRefCodeBlock(codeRef)
      console.log(refCodeBlock)

      const result = compareCodeBlocks(docCodeBlock, refCodeBlock)

      // TODO: Compare source code block with the referenced code block.
      // - Maybe we want to regex the whole file contents instead of specifiying the lines?
      //   We can also add .* at the right places (i.e. start and end).
      //   This way we don't have to fix line numbers each time we change the example code,
      //   which sounds like a major hassle.
      //   This does make the whole code reffing potentially a bit imprecise, but it might be ok
      //   in practice.
      //   We could do both: we take part specified by lines and we try to match regex inside of it.
      //   And if you don't specify the lines, we take the whole file.
      // - We should look into some kind of minifying as a method of normalizing the code for
      //   comparison. Ideally it would take care of whitespaces and comments (oh yes comments for
      //   sure!), while not e.g. changing names. We should probably do this at language level: if
      //   block is js/ts -> use this minifier.
      // - When comparing code blocks, be robust about indentation shift and trim at start and end
      //   (whitespace, newlines), unless this is already covered by minification.
      // - Regarding holes (...) -> I don't think minifiers can parse those.
      //   Solution is probably to use `// ...` instead. We can say if js/ts file has
      //   a line that matches `^\s*//\s*\.\.\.\s*$`, that is a hole, and we will take that into
      //   account when comparing the code blocks.

      // TODO: What about versioned docs? Should we check here if file is from there and ignore it
      //   if so? Either that, or we have to tell it from which git reference to pull the code from.
      //   E.g. based on wasp version tag. Check versions.json .
      //   Probably best to skip them for now.

      // IDEAS:
      // - Add support for specifing the awk string to do transformation.
    })
  }
}

export default plugin

/**
 * Given a code block ast node that has `ref="..."` in its meta field, it will return the string
 * between the quotes. Code block's meta field is everything after initial triple backticks of the
 * code block, in the same line.
 */
function obtainCodeRefStringFromCodeBlock(node: Code): string | null {
  return node.meta?.match(/\bref="(.+?)"/)?.[1]
}

interface CodeRef {
  filePathFromRepoRoot: string
  startLine: number
  endLine: number
}

/**
 * Parses a code ref string of shape "<filepath>:L<start_line>:<end_line>".
 * Example of a valid string: "waspc/examples/todoApp/src/operations.ts:L42-314".
 * @throws When parsing fails.
 */
function parseCodeRefString(codeRefString: string): CodeRef {
  const matches = codeRefString.match(/^(.+?):L(\d+)-(\d+)$/)
  if (!matches) throwInvalidRefAttributeError()

  const [, filePathFromRepoRoot, startLineStr, endLineStr] = matches
  const startLine: number | null = parseNonNegativeInteger(startLineStr)
  const endLine: number | null = parseNonNegativeInteger(endLineStr)
  if (startLine === null || endLine === null) throwInvalidRefAttributeError()

  return { filePathFromRepoRoot, startLine, endLine }

  function throwInvalidRefAttributeError() {
    throw new Error(
      `Your code block ref is not valid.`
        + ` Expected "<filepath>:L<start_line>-<end_line>"`
        + `, but got "${codeRefString}".`
    )
  }
}

function parseNonNegativeInteger(str: string): number | null {
  const result = Number(str)
  return Number.isInteger(result) && result >= 0 ? result : null
}

function fetchCodeRefCodeBlock(codeRef: CodeRef): string {
  const rootDir = getRepoRootDir()
  const codeRefAbsolutePath = path.join(rootDir, codeRef.filePathFromRepoRoot)

  return fs
    .readFileSync(codeRefAbsolutePath, 'utf8')
    .split('\n')
    .slice(codeRef.startLine - 1, codeRef.endLine)
    .join('\n')
}

/**
 * TODO: Make this not be hardcoded, instead look for `.git` dir or something like that.
 * NOTE:
 * We rely on this file being at the specific position in the code base.
 * If that changes, we need to update this function.
 */
function getRepoRootDir(): string {
  const moduleAbsolutePath = url.fileURLToPath(import.meta.url)

  const remarkDir = path.dirname(moduleAbsolutePath)
  const srcDir = path.dirname(remarkDir)
  const webDir = path.dirname(srcDir)

  return path.dirname(webDir)
}

function compareCodeBlocks(
  docCodeBlock: string,
  refCodeBlock: string
): boolean {
  return docCodeBlock === refCodeBlock
}
