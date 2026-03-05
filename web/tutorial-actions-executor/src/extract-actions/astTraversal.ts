import type { Root } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { visit } from "unist-util-visit";

import type { Action, ActionId } from "../actions/actions.js";

export const TUTORIAL_ACTION_NODE_NAME = "TutorialAction";

export type TutorialActionNode = {
  id: ActionId;
  action: Action["kind"];
  starterTemplateName: string | null;
};

export function extractTutorialActionNodes(ast: Root): TutorialActionNode[] {
  const nodes: TutorialActionNode[] = [];

  visit(ast, "mdxJsxFlowElement", (node) => {
    if (node.name === TUTORIAL_ACTION_NODE_NAME) {
      nodes.push(
        createTutorialActionNode(
          getAttributeValue(node, "id"),
          getAttributeValue(node, "action"),
          getAttributeValue(node, "starterTemplateName"),
        ),
      );
    }
  });

  return nodes;
}

export function createTutorialActionNode(
  id: string | null,
  action: string | null,
  starterTemplateName: string | null,
): TutorialActionNode {
  if (!id) {
    throw new Error(`TutorialAction component requires the 'id' attribute.`);
  }

  if (!action) {
    throw new Error(
      `TutorialAction component requires the 'action' attribute.`,
    );
  }

  return {
    id: id as ActionId,
    action: action as Action["kind"],
    starterTemplateName,
  };
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
