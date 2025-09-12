import fs from "fs/promises";
import path from "path";

import * as acorn from "acorn";
import { fromMarkdown } from "mdast-util-from-markdown";
import { mdxJsxFromMarkdown, type MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { mdxJsx } from "micromark-extension-mdx-jsx";
import { visit } from "unist-util-visit";

import type {
  Action,
  ActionCommon,
  ActionId,
  MarkdownFilePath,
} from "../actions/actions.js";
import {
  createApplyPatchAction,
  createMigrateDbAction,
} from "../actions/index.js";
import type { TutorialDirPath } from "../tutorialApp.js";

export async function getActionsFromTutorialFiles(
  tutorialDir: TutorialDirPath,
): Promise<Action[]> {
  const tutorialFilePaths = await getTutorialFilePaths(tutorialDir);
  const actions: Action[] = [];

  for (const filePath of tutorialFilePaths) {
    const fileActions = await getActionsFromMarkdownFile(filePath);
    actions.push(...fileActions);
  }

  return actions;
}

async function getTutorialFilePaths(
  tutorialDir: TutorialDirPath,
): Promise<MarkdownFilePath[]> {
  const files = await fs.readdir(tutorialDir);
  return (
    files
      .filter((file) => file.endsWith(".md"))
      // Tutorial files are named "01-something.md"
      // and we want to sort them by the number prefix
      .sort((a, b) => {
        const aNumber = parseInt(a.split("-")[0]!, 10);
        const bNumber = parseInt(b.split("-")[0]!, 10);
        return aNumber - bNumber;
      })
      .map((file) => path.resolve(tutorialDir, file) as MarkdownFilePath)
  );
}

async function getActionsFromMarkdownFile(
  tutorialFilePath: MarkdownFilePath,
): Promise<Action[]> {
  const actions: Action[] = [];
  const fileContent = await fs.readFile(path.resolve(tutorialFilePath));

  const ast = fromMarkdown(fileContent, {
    extensions: [mdxJsx({ acorn, addResult: true })],
    mdastExtensions: [mdxJsxFromMarkdown()],
  });

  const tutorialComponentName = "TutorialAction";
  visit(ast, "mdxJsxFlowElement", (node) => {
    if (node.name !== tutorialComponentName) {
      return;
    }
    const actionId = getAttributeValue(node, "id") as ActionId | null;
    const actionName = getAttributeValue(node, "action");

    if (!actionId) {
      throw new Error(
        `TutorialAction component requires the 'id' attribute. File: ${tutorialFilePath}`,
      );
    }

    if (!actionName) {
      throw new Error(
        `TutorialAction component requires the 'action' attribute. File: ${tutorialFilePath}`,
      );
    }

    actions.push(
      createAction(actionName, {
        id: actionId,
        tutorialFilePath,
      }),
    );
  });

  return actions;
}

function createAction(actionName: string, commonData: ActionCommon): Action {
  const actionCreators: Record<string, (data: ActionCommon) => Action> = {
    APPLY_PATCH: createApplyPatchAction,
    MIGRATE_DB: createMigrateDbAction,
  };
  const createFn = actionCreators[actionName];
  if (!createFn) {
    throw new Error(`Unknown action type: ${actionName}`);
  }
  return createFn(commonData);
}

function getAttributeValue(
  node: MdxJsxFlowElement,
  attributeName: string,
): string | null {
  const attribute = node.attributes.find(
    (attr) => attr.type === "mdxJsxAttribute" && attr.name === attributeName,
  );
  return attribute && typeof attribute.value === "string"
    ? attribute.value
    : null;
}
