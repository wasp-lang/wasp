import type * as hast from "hast";
import assert from "node:assert";

export function hasClass(element: hast.Element, className: string): boolean {
  return getClassNames(element).includes(className);
}

export function getClassNames(element: hast.Element): string[] {
  const className = element.properties?.className;
  return Array.isArray(className) ? className.map(String) : [];
}

export function hastTextContent(node: hast.Nodes): string {
  if (node.type === "text") {
    return node.value;
  }
  assert("children" in node, "Unknown node text content.");

  return node.children.map(hastTextContent).join("");
}
