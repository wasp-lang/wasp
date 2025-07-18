import fs from "fs/promises";
import path from "path";

import * as acorn from "acorn";
import { fromMarkdown } from "mdast-util-from-markdown";
import { mdxJsxFromMarkdown, type MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { mdxJsx } from "micromark-extension-mdx-jsx";
import { visit } from "unist-util-visit";

import type {
  MarkdownFilePath,
  StepName,
  TutorialDirPath,
} from "../brandedTypes.js";
import {
  createApplyPatchAction,
  createMigrateDbAction,
  type Action,
  type ActionCommon,
} from "../executeSteps/actions.js";
import { log } from "../log.js";

export async function getActionsFromTutorialFiles(
  tutorialDir: TutorialDirPath,
): Promise<Action[]> {
  const tutorialFilePaths = await getTutorialFilePaths(tutorialDir);
  const actions: Action[] = [];

  for (const filePath of tutorialFilePaths) {
    const fileActions = await getActionsFromMarkdownFile(filePath);
    actions.push(...fileActions);
  }

  log("success", `Found ${actions.length} actions in tutorial files.`);
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
  sourceFilePath: MarkdownFilePath,
): Promise<Action[]> {
  const actions: Action[] = [];
  const fileContent = await fs.readFile(path.resolve(sourceFilePath));

  const ast = fromMarkdown(fileContent, {
    extensions: [mdxJsx({ acorn, addResult: true })],
    mdastExtensions: [mdxJsxFromMarkdown()],
  });

  const tutorialComponentName = "TutorialAction";
  visit(ast, "mdxJsxFlowElement", (node) => {
    if (node.name !== tutorialComponentName) {
      return;
    }
    const stepName = getAttributeValue(node, "step") as StepName | null;
    const actionName = getAttributeValue(node, "action");

    if (!stepName || !actionName) {
      throw new Error("Step and action attributes are required");
    }

    actions.push(
      createAction(actionName, {
        stepName,
        sourceFilePath,
      }),
    );
  });

  return actions;
}

function createAction(actionName: string, commonData: ActionCommon): Action {
  const actionCreators: Record<string, (data: ActionCommon) => Action> = {
    "apply-patch": createApplyPatchAction,
    "migrate-db": createMigrateDbAction,
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
