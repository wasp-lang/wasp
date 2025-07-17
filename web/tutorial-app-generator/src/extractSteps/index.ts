import fs from "fs/promises";
import path from "path";

import * as acorn from "acorn";
import { fromMarkdown } from "mdast-util-from-markdown";
import { mdxJsxFromMarkdown, type MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { mdxJsx } from "micromark-extension-mdx-jsx";
import { visit } from "unist-util-visit";

import {
  createApplyPatchAction,
  type Action,
  type ActionCommon,
} from "../executeSteps/actions.js";
import { log } from "../log.js";

const componentName = "TutorialAction";

export async function getActionsFromTutorialFiles(): Promise<Action[]> {
  const files = await fs
    .readdir(path.resolve("../docs/tutorial"))
    .then((files) =>
      files
        .filter((file) => file.endsWith(".md"))
        .sort((a, b) => {
          const aNumber = parseInt(a.split("-")[0]!, 10);
          const bNumber = parseInt(b.split("-")[0]!, 10);
          return aNumber - bNumber;
        }),
    );
  const actions: Action[] = [];
  for (const file of files) {
    const fileActions = await getActionsFromMarkdownFile(
      path.resolve("../docs/tutorial", file),
    );
    actions.push(...fileActions);
  }
  log("success", `Found ${actions.length} actions in tutorial files.`);
  return actions;
}

async function getActionsFromMarkdownFile(filePath: string): Promise<Action[]> {
  const actions = [] as Action[];
  const doc = await fs.readFile(path.resolve(filePath));

  const ast = fromMarkdown(doc, {
    extensions: [mdxJsx({ acorn, addResult: true })],
    mdastExtensions: [mdxJsxFromMarkdown()],
  });

  visit(ast, "mdxJsxFlowElement", (node) => {
    if (node.name !== componentName) {
      return;
    }
    const step = getAttributeValue(node, "step");
    const action = getAttributeValue(node, "action");

    if (!step || !action) {
      throw new Error("Step and action attributes are required");
    }

    const commonActionData: ActionCommon = {
      stepName: step,
      markdownSourceFilePath: filePath,
    };

    if (action === "migrate-db") {
      actions.push({
        ...commonActionData,
        kind: "migrate-db",
      });
      return;
    } else if (action === "apply-patch") {
      const patchAction = createApplyPatchAction(commonActionData);
      actions.push(patchAction);
      return;
    }

    throw new Error(
      `Unknown action type: ${action} in file: ${filePath}, step: ${step}`,
    );
  });

  return actions;
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
