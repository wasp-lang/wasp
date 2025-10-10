import fs from "fs/promises";
import path from "path";

import * as acorn from "acorn";
import { fromMarkdown } from "mdast-util-from-markdown";
import { mdxJsxFromMarkdown, type MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { mdxJsx } from "micromark-extension-mdx-jsx";
import { visit } from "unist-util-visit";

import type {
  Action,
  ActionId,
  BaseAction,
  MdxFilePath,
} from "../actions/actions.js";
import {
  createApplyPatchAction,
  createInitAppAction,
  createMigrateDbAction,
} from "../actions/index.js";
import { assertUnreachable } from "../assert.js";
import type { TutorialApp, TutorialDirPath } from "../tutorialApp.js";

export async function getActionsFromTutorialFiles(
  tutorialApp: TutorialApp,
): Promise<Action[]> {
  const tutorialFilePaths = await getTutorialFilePaths(
    tutorialApp.docsTutorialDirPath,
  );
  const actions: Action[] = [];

  for (const filePath of tutorialFilePaths) {
    const fileActions = await getActionsFromMdxFile(filePath, tutorialApp);
    actions.push(...fileActions);
  }

  return actions;
}

async function getTutorialFilePaths(
  tutorialDir: TutorialDirPath,
): Promise<MdxFilePath[]> {
  const files = await fs.readdir(tutorialDir);
  return sortTutorialFileNames(filterTutorialFileNames(files)).map(
    (file) => path.resolve(tutorialDir, file) as MdxFilePath,
  );
}

export function filterTutorialFileNames(filePaths: string[]): string[] {
  return filePaths.filter((file) => file.endsWith(".md"));
}

/**
 * Tutorial files are named "01-something.md" and we want to sort them by the number prefix.
 * @param filePaths
 * @returns Sorted file paths
 */
export function sortTutorialFileNames(filePaths: string[]): string[] {
  return filePaths.sort((a, b) => {
    const aNumber = parseInt(a.split("-")[0]!, 10);
    const bNumber = parseInt(b.split("-")[0]!, 10);
    return aNumber - bNumber;
  });
}

async function getActionsFromMdxFile(
  sourceTutorialFilePath: MdxFilePath,
  tutorialApp: TutorialApp,
): Promise<Action[]> {
  const fileContent = await fs.readFile(path.resolve(sourceTutorialFilePath));
  return getActionsFromMdxContent(
    sourceTutorialFilePath,
    fileContent,
    tutorialApp,
  );
}

export async function getActionsFromMdxContent(
  sourceTutorialFilePath: MdxFilePath,
  fileContent: Buffer<ArrayBufferLike>,
  tutorialApp: TutorialApp,
): Promise<Action[]> {
  const actions: Action[] = [];

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
    const actionName = getAttributeValue(node, "action") as
      | Action["kind"]
      | null;
    const waspStarterTemplateName = getAttributeValue(
      node,
      "starterTemplateName",
    );

    if (!actionId) {
      throw new Error(
        `TutorialAction component requires the 'id' attribute. File: ${sourceTutorialFilePath}`,
      );
    }

    if (!actionName) {
      throw new Error(
        `TutorialAction component requires the 'action' attribute. File: ${sourceTutorialFilePath}`,
      );
    }

    const commonData: BaseAction = {
      id: actionId,
      sourceTutorialFilePath,
    };
    switch (actionName) {
      case "INIT_APP":
        if (waspStarterTemplateName === null) {
          throw new Error(
            `TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute. File: ${sourceTutorialFilePath}`,
          );
        }
        actions.push(createInitAppAction(commonData, waspStarterTemplateName));
        break;
      case "APPLY_PATCH":
        actions.push(
          createApplyPatchAction(
            commonData,
            tutorialApp.docsTutorialPatchesPath,
          ),
        );
        break;
      case "MIGRATE_DB":
        actions.push(createMigrateDbAction(commonData));
        break;
      default:
        assertUnreachable(
          actionName,
          `Unknown action '${actionName}' in TutorialAction component. File: ${sourceTutorialFilePath}`,
        );
    }
  });

  return actions;
}

export function getAttributeValue(
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
