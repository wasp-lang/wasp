import type { Root } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { visit } from "unist-util-visit";

import type { Action, ActionId } from "../actions/actions.ts";

export const TUTORIAL_ACTION_NODE_NAME = "TutorialAction";

export type TutorialActionNode = {
  id: ActionId;
  action: Action["kind"];
  starterTemplateName: string | null;
  packageSpecifiers: string[] | null;
};

export function extractTutorialActionNodes(ast: Root): TutorialActionNode[] {
  const nodes: TutorialActionNode[] = [];

  visit(ast, "mdxJsxFlowElement", (node) => {
    if (node.name === TUTORIAL_ACTION_NODE_NAME) {
      nodes.push(
        createTutorialActionNode({
          id: getAttributeValue(node, "id"),
          action: getAttributeValue(node, "action"),
          starterTemplateName: getAttributeValue(node, "starterTemplateName"),
          packageSpecifiers: getAttributeValue(node, "packageSpecifiers"),
        }),
      );
    }
  });

  return nodes;
}

/** The raw attribute values of a `<TutorialAction>` element. */
export type TutorialActionAttributes = {
  id: string | null;
  action: string | null;
  starterTemplateName: string | null;
  /** Space-separated, like the arguments of `npm install`. */
  packageSpecifiers: string | null;
};

export function createTutorialActionNode({
  id,
  action,
  starterTemplateName,
  packageSpecifiers,
}: TutorialActionAttributes): TutorialActionNode {
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
    packageSpecifiers:
      packageSpecifiers === null ? null : splitBySpaces(packageSpecifiers),
  };
}

function splitBySpaces(text: string): string[] {
  return text.split(/\s+/).filter(Boolean);
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
