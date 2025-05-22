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

import type * as md from "mdast";
import type {} from "mdast-util-mdx"; // Type-only empty import to register MDX types into mdast
import * as path from "node:path";
import * as prettier from "prettier";
import { blankSourceFile } from "ts-blank-space";
import * as ts from "typescript";
import type { Plugin } from "unified";
import { visit } from "unist-util-visit";
import {
  assertSupportedLanguage,
  checkNodeIsCodeWithMeta,
} from "./util/code-blocks";

// Wrapped in \b to denote a word boundary
const META_FLAG_REGEX = /\bauto-js\b/;
const SUPPORTED_LANGS = new Set(["ts", "tsx"] as const);

const autoJSCodePlugin: Plugin<[], md.Root> = () => async (tree, file) => {
  // `visit` does not allow async visitors, so
  // we will just store the needed transformations
  // in an array and run them after the visit is done.
  const pendingCodeBlockTransforms: (() => Promise<void>)[] = [];

  visit(tree, checkNodeIsCodeWithMeta(META_FLAG_REGEX), (node, idx, parent) => {
    assertSupportedLanguage(node, file, SUPPORTED_LANGS);

    // Can't do async here, just store the transformation to run later
    pendingCodeBlockTransforms.push(async () => {
      // Remove our flag from the meta so other plugins don't trip up
      const newMeta = node.meta.replace(META_FLAG_REGEX, "");

      const jsCodeBlock = await makeJsCodeBlock(newMeta, node);
      const tsCodeBlock = await makeTsCodeBlock(newMeta, node);

      // The specific structure of the new node was retrieved by copy-pasting
      // an example into the MDX playground and inspecting the AST.
      // https://mdxjs.com/playground
      const newNode: md.RootContent = {
        type: "mdxJsxFlowElement",
        name: "Tabs",
        attributes: [
          { type: "mdxJsxAttribute", name: "groupId", value: "js-ts" },
        ],
        children: [
          {
            type: "mdxJsxFlowElement",
            name: "TabItem",
            attributes: [
              { type: "mdxJsxAttribute", name: "value", value: "js" },
              { type: "mdxJsxAttribute", name: "label", value: "JavaScript" },
            ],
            children: [jsCodeBlock],
          },
          {
            type: "mdxJsxFlowElement",
            name: "TabItem",
            attributes: [
              { type: "mdxJsxAttribute", name: "value", value: "ts" },
              { type: "mdxJsxAttribute", name: "label", value: "TypeScript" },
            ],
            children: [tsCodeBlock],
          },
        ],
      };

      // Replace input node for the new one in the parent's children array
      parent.children.splice(idx, 1, newNode);
    });
  });

  // We're out of the visitor, now we can run the transforms!
  for (const fn of pendingCodeBlockTransforms) {
    await fn();
  }
};

export default autoJSCodePlugin;

// Taken from Docusaurus
// https://github.com/facebook/docusaurus/blob/v2.4.3/packages/docusaurus-theme-common/src/utils/codeBlockUtils.ts
const CODE_BLOCK_TITLE_REGEX = /title=(?<quote>["'])(?<title>.*?)\1/;

async function makeJsCodeBlock(
  metaString: string,
  node: md.Code,
): Promise<md.Code> {
  // Find the `title=` meta param and change the extension
  const meta = metaString.replace(
    CODE_BLOCK_TITLE_REGEX,
    (_fullMatch, _quote, title) =>
      `title=${JSON.stringify(
        transformExt(title, (ext) => ext.replace("ts", "js")),
      )}`,
  );
  const lang = node.lang?.replace("ts", "js");
  const isJsx = node.lang.endsWith("x");
  const code = await format(convertToJs(node.value, { isJsx }), {
    parser: "babel",
  });

  return {
    type: "code",
    value: code,
    lang: lang,
    meta: meta,
  };
}

async function makeTsCodeBlock(
  metaString: string,
  node: md.Code,
): Promise<md.Code> {
  const lang = node.lang;
  const code = await format(node.value, { parser: "babel-ts" });

  return {
    type: "code",
    value: code,
    lang: lang,
    meta: metaString,
  };
}

function convertToJs(code: string, { isJsx }: { isJsx: boolean }) {
  // We create a source file from ts so that way we can specify if
  // we want to use JSX or not, because the parsing is different.
  // Copied from the ts-blank-space playground
  // https://github.com/bloomberg/ts-blank-space/blob/4102b1f26b1c53d38a1de74c10f262af5fd34fe8/website/play/play-utils.ts#L4
  const sourceFile = ts.createSourceFile(
    "input.ts",
    code,
    { languageVersion: ts.ScriptTarget.ESNext },
    true,
    isJsx ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
  );

  return blankSourceFile(sourceFile);
}

let prettierConfig: prettier.Options | undefined;

async function format(
  code: string,
  { parser }: { parser: prettier.Options["parser"] },
) {
  prettierConfig ??= await prettier.resolveConfig(__dirname, {
    useCache: true,
    editorconfig: true,
  });
  const formatted = await prettier.format(code, { ...prettierConfig, parser });
  return formatted.trim(); // prettier adds a trailing newline, we remove it
}

function transformExt(inPath: string, fn: (ext: string) => string) {
  const inExt = path.extname(inPath);
  const outExt = fn(inExt);
  const outPath = inPath.slice(0, -inExt.length) + outExt;
  return outPath;
}
