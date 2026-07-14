import type * as hast from "hast";

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
  if ("children" in node) {
    return node.children.map(hastTextContent).join("");
  }

  throw new Error("Unknown node text content.");
}
