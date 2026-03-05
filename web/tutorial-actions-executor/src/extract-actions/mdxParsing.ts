import fs from "fs/promises";
import path from "path";

import * as acorn from "acorn";
import { fromMarkdown } from "mdast-util-from-markdown";
import { mdxJsxFromMarkdown } from "mdast-util-mdx-jsx";
import { mdxJsx } from "micromark-extension-mdx-jsx";

import type { Action, MdxFilePath } from "../actions/actions.js";
import type { Branded } from "../brandedTypes.js";
import type { PatchesDirPath } from "../tutorialApp.js";
import { extractTutorialActionNodes } from "./astTraversal.js";
import { mapTutorialActionNodeToAction } from "./nodeMapping.js";

type MdxContent = Branded<Buffer<ArrayBufferLike>, "MdxContent">;

export async function getActionsFromMdxFile(
  sourceTutorialFilePath: MdxFilePath,
  patchesPath: PatchesDirPath,
): Promise<Action[]> {
  const fileContent = await fs.readFile(path.resolve(sourceTutorialFilePath));

  try {
    return getActionsFromMdxContent(
      fileContent as MdxContent,
      sourceTutorialFilePath,
      patchesPath,
    );
  } catch (error: unknown) {
    if (error instanceof Error) {
      throw new Error(`${error.message} (File: ${sourceTutorialFilePath})`);
    }
    throw error;
  }
}

export async function getActionsFromMdxContent(
  fileContent: MdxContent,
  sourceTutorialFilePath: MdxFilePath,
  patchesPath: PatchesDirPath,
): Promise<Action[]> {
  const ast = fromMarkdown(fileContent, {
    extensions: [mdxJsx({ acorn, addResult: true })],
    mdastExtensions: [mdxJsxFromMarkdown()],
  });

  const tutorialActionNodes = extractTutorialActionNodes(ast);

  return tutorialActionNodes.map((node) =>
    mapTutorialActionNodeToAction(node, sourceTutorialFilePath, patchesPath),
  );
}
