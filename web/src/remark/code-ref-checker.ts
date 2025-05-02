import type { Root } from 'mdast'
import type { Plugin } from 'unified'
import { visit } from 'unist-util-visit'

const plugin: Plugin<[], Root> = () => {
  return (tree, _file) => {
    visit(tree, 'code', (node) => {
      if (!node.meta?.includes('ref=')) {
        return
      }

      // 1. Parse the `ref={...}` from the code block (node.meta).
      // - Example: `ref={file: "wasp/waspc/examples/todoApp/src/operations.ts", lines: [7, 42], awk: "<awk_command>"}`
      // - File path should start relative from the wasp monorepo dir.
      // - Ok, to be able to parse this, we will probably want to be smart by actually using JSON
      //   parser to parse the JSON object, so it stops on time and stuff (and doesn't consume
      //   the whole node.meta).
      // - every code block needs to have a `ref`, otherwise we throw an error, but, they can do
      //   `ref=null` if they want to skip it.
      //   - what about the versioned_docs and other stuff? Who will add ref=null to all of that.
      // - Actually this is better:
      //   - `ref="waspc/examples/todoApp/src/operations.ts:L7-42"`
      //   - and if we want something extra, we can try to also cram it in or just go like `ref-awk="<awk_command>"`.

      // 2. Fetch code block from the referenced file.

      // 3. Compare source code block with the referenced code block.
      // - When comparing code blocks, be robust about indentation shift and trim at start and end
      //   (whitespace, newlines). We can even minify it maybe (without changing the names
      //   probably). I guess what we really want to do is to normalize them, and the question is
      //   how. Don't minify.

      // IDEAS:
      // - Is it too ugly to have all this coderef data in the first line of code block?
      //   If so, we can look into handling it differently: e.g. putting it into an mdx(js) comment
      //   above the code block. This might also make parsing easier, and allows us to go multi-line
      //   if we want. Or, a comment inside the actual code block, but at the very start, that we
      //   process and then strip out.
      // - To shorten the file paths, we can look into offering a couple of common replacements.
      //   For example if most of the paths will start with "wasp/waspc/examples/", we can introduce
      //   `"$exs" -> "wasp/waspc/examples/"` replacement.
      //   - Bad side is that it is kind of magical, so e.g. we loose copy-pastability.
      //   - let's do it only if we really get annoyed with it.
    })
  }
}

export default plugin
