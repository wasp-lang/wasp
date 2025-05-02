import type { Code, Root } from 'mdast'
import type { Plugin } from 'unified'
import { SKIP, visit } from 'unist-util-visit'

// TODO: Add explanation of what this plugin is about. Examples.

const plugin: Plugin<[], Root> = () => {
  return (tree, _file) => {
    visit(tree, 'code', (codeBlockNode) => {
      const codeRefString = obtainCodeRefStringFromCodeBlock(codeBlockNode);
      if (!codeRefString) return SKIP

      const codeRef = parseCodeRefString(codeRefString);
      console.log(codeRef);

      // TODO: Fetch code block from the referenced file.

      // TODO: Compare source code block with the referenced code block.
      // - When comparing code blocks, be robust about indentation shift and trim at start and end
      //   (whitespace, newlines). We can even minify it maybe (without changing the names
      //   probably). I guess what we really want to do is to normalize them, and the question is
      //   how. Don't minify.

      // TODO: What about versioned docs? Should we check here if file is from there and ignore it
      //   if so? Either that, or we have to tell it from which git reference to pull the code from.
      //   Probably best to skip them for now.

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
      // - Add support for specifing the awk string to do transformation.
    })
  }
}

export default plugin

// Given a code block ast node that has `ref="..."` in its meta field, it will return the string
// between the quotes.
// Code block's meta field is everything after initial triple backticks of the code block, in the
// same line.
function obtainCodeRefStringFromCodeBlock (node: Code): string | null {
  return node.meta?.match(/\bref="(.+?)"/)?.[1];
}

interface CodeRef {
  filePath: string,
  startLine: number,
  endLine: number
}

// Parses a code ref string of shape "<filepath>:L<start_line>:<end_line>".
// Example of a valid string: "waspc/examples/todoApp/src/operations.ts:L42-314".
// Throws if parsing failed.
function parseCodeRefString (codeRefString: string): CodeRef {
  const matches = codeRefString.match(/^(.+?):L(\d+)-(\d+)$/);
  if (!matches) throwInvalidRefAttributeError();

  const [, filePath, startLineStr, endLineStr] = matches;
  const startLine: number | null = parseNonNegativeInteger(startLineStr);
  const endLine: number | null = parseNonNegativeInteger(endLineStr);
  if (startLine === null || endLine === null) throwInvalidRefAttributeError();

  return { filePath, startLine, endLine };

  function throwInvalidRefAttributeError () {
    throw new Error(
      `Your code block ref is not valid.`
        + ` Expected "<filepath>:L<start_line>-<end_line>"`
        + `, but got "${codeRefString}".`
    );
  }
}

function parseNonNegativeInteger (str: string): number | null {
  const result = Number(str);
  return (Number.isInteger(result) && result >= 0) ? result : null;
}
