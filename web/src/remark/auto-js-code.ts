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

import type * as md from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx";
import * as path from "node:path";
import { blankSourceFile } from "ts-blank-space";
import * as ts from "typescript";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import {
  assertCodeBlockIsInLanguage,
  codeBlockMetaStringFromMap,
  codeBlockMetaStringToMap,
  formatCodeBlock,
  makeCheckForCodeWithMeta,
} from "./util/code-blocks";

const META_FLAG = "auto-js";

interface LanguageTransformationInfo {
  jsx: boolean;
  outputLang: string;
}

const LANGUAGE_TRANSFORMATIONS = {
  ts: { jsx: false, outputLang: "js" },
  tsx: { jsx: true, outputLang: "jsx" },
} satisfies Record<string, LanguageTransformationInfo>;

const EXTENSION_TRANSFORMATIONS = {
  ".ts": ".js",
  ".tsx": ".jsx",
};

const isCodeWithAutoJsFlag = makeCheckForCodeWithMeta(META_FLAG);
const supportedLanguages = Object.keys(
  LANGUAGE_TRANSFORMATIONS,
) as readonly (keyof typeof LANGUAGE_TRANSFORMATIONS)[];

const autoJSCodePlugin: Plugin<[], md.Root> = () => async (tree, file) => {
  // `visit` does not allow async visitors, so we will just store the needed transformations
  // in an array and run them after the visit is done.
  const pendingCodeBlockTransforms: (() => Promise<void>)[] = [];

  visit(tree, isCodeWithAutoJsFlag, (node, idx, parent) => {
    // Can't do async here, just store the transformation to run later.
    pendingCodeBlockTransforms.push(async () => {
      try {
        assertCodeBlockIsInLanguage(node, supportedLanguages);
        const transformationInfo = LANGUAGE_TRANSFORMATIONS[node.lang];

        const jsCodeBlock = await makeFormattedJsCodeBlock(
          node,
          transformationInfo,
        );
        const tsCodeBlock = await formatCodeBlock(node, { parser: "babel-ts" });

        const newNode = createTabbedCodeBlocks({ jsCodeBlock, tsCodeBlock });

        // Replace input node for the new one in the parent's children array.
        parent.children.splice(idx, 1, newNode);
      } catch (err) {
        // We catch any thrown errors and annotate them as file errors, with the
        // code block position.
        file.fail(err, { place: node.position });
      }
    });
  });

  // We're out of the visitor, now we can run the transforms!
  for (const fn of pendingCodeBlockTransforms) {
    await fn();
  }
};

export default autoJSCodePlugin;

function createTabbedCodeBlocks({
  jsCodeBlock,
  tsCodeBlock,
}: {
  jsCodeBlock: md.Code;
  tsCodeBlock: md.Code;
}): MdxJsxFlowElement {
  // The specific structure of the new node was retrieved by copy-pasting
  // an example into the MDX playground and inspecting the AST.
  // https://mdxjs.com/playground
  return {
    type: "mdxJsxFlowElement",
    name: "Tabs",
    attributes: [{ type: "mdxJsxAttribute", name: "groupId", value: "js-ts" }],
    children: [
      {
        type: "mdxJsxFlowElement",
        name: "TabItem",
        attributes: [
          { type: "mdxJsxAttribute", name: "value", value: "js" },
          {
            type: "mdxJsxAttribute",
            name: "label",
            value: "JavaScript",
          },
        ],
        children: [jsCodeBlock],
      },
      {
        type: "mdxJsxFlowElement",
        name: "TabItem",
        attributes: [
          { type: "mdxJsxAttribute", name: "value", value: "ts" },
          {
            type: "mdxJsxAttribute",
            name: "label",
            value: "TypeScript",
          },
        ],
        children: [tsCodeBlock],
      },
    ],
  };
}

async function makeFormattedJsCodeBlock(
  originalNode: md.Code,
  transformationInfo: LanguageTransformationInfo,
): Promise<md.Code> {
  const meta = codeBlockMetaStringToMap(originalNode.meta!);

  // Find the `title=` meta param and change the extension.
  const originalTitle = meta.get("title");
  if (originalTitle) {
    const newTitle = transformExt(originalTitle, EXTENSION_TRANSFORMATIONS);
    meta.set("title", newTitle);
  }

  const newMetaString = codeBlockMetaStringFromMap(meta);

  const newLang = transformationInfo.outputLang;

  const newCode = convertToJs(originalNode.value, transformationInfo);

  const newNode = {
    ...originalNode,
    meta: newMetaString,
    lang: newLang,
    value: newCode,
  };

  const formattedNode = await formatCodeBlock(newNode, { parser: "babel" });

  return formattedNode;
}

/*
  For the conversion to JS we use the `ts-blank-space` library, which is a
  TypeScript parser that can convert TS to JS -- by **only** removing the types.
  This is useful because we want to keep the code as close to the original as
  possible, and we don't run a full, slow TS compiler on it.

  We need to use `prettier` to format the code afterwards, because `ts-blank-space`
  leaves white space where types used to be, and we want to remove that.
*/
function convertToJs(tsCode: string, { jsx }: LanguageTransformationInfo) {
  // We create a source file from ts so that way we can specify if we want to use
  // JSX or not, because the parsing is different. This is only done in memory.
  // Copied from the ts-blank-space playground.
  // https://github.com/bloomberg/ts-blank-space/blob/4102b1f26b1c53d38a1de74c10f262af5fd34fe8/website/play/play-utils.ts#L4
  const sourceFile = ts.createSourceFile(
    "input.ts",
    tsCode,
    { languageVersion: ts.ScriptTarget.ESNext },
    true,
    jsx ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
  );

  return blankSourceFile(sourceFile);
}

function transformExt(
  inputFilePath: string,
  transformations: Partial<Record<string, string>>,
) {
  const inputExt = path.extname(inputFilePath);
  const outputExt = transformations[inputExt];

  if (!outputExt) {
    throw new Error(
      `Unsupported file extension: ${inputExt}. Please use one of: ${Object.keys(
        EXTENSION_TRANSFORMATIONS,
      ).join(", ")}`,
    );
  }

  const outputFilePath = inputFilePath.slice(0, -inputExt.length) + outputExt;
  return outputFilePath;
}
