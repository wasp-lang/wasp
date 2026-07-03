/**
 * This module maps the React elements users provide in `app.head` to the JSX
 * source strings the generator interpolates into the SDK's `Layout` component.
 *
 * The spec file is typechecked before it is evaluated, so by the time the
 * mapping runs we can trust that `head` holds React elements. What the type
 * system can't express is that only host elements (e.g. `<meta />`,
 * `<link />`) with serializable props survive the trip through the compiler's
 * JSON intermediate representation — components and function props typecheck
 * as `ReactElement` just fine — so those two restrictions are enforced here.
 */

import type { ReactElement } from "react";
import { SpecUserError } from "./specUserError.js";

export function mapHeadElements(head: ReactElement[]): string[] {
  return head.map((element, index) => {
    const path = `app.head[${index}]`;
    // The type system already enforces this, but values that dodge it (e.g.
    // through `as any` or untyped imports) would otherwise produce a
    // misleading error about the element's `type` field.
    if (typeof element !== "object" || element === null) {
      throw new SpecUserError(
        `Invalid \`${path}\`: expected a JSX element, got ${describeValue(element)}.`,
      );
    }
    return printElement(element, path);
  });
}

const REACT_FRAGMENT_SYMBOL = Symbol.for("react.fragment");

/**
 * Prints a React node (element, fragment, text...) as JSX source.
 * Returns `null` for nodes React would not render (null, undefined, booleans).
 */
function printNode(node: unknown, path: string): string | null {
  if (node === null || node === undefined || typeof node === "boolean") {
    return null;
  }
  if (typeof node === "string" || typeof node === "number") {
    assertJsonSerializable(node, path);
    return `{${JSON.stringify(node)}}`;
  }
  if (Array.isArray(node)) {
    const printedChildren = node
      .map((child, index) => printNode(child, `${path}[${index}]`))
      .filter((printedChild) => printedChild !== null);
    return printedChildren.length > 0 ? printedChildren.join("") : null;
  }
  if (typeof node === "object") {
    return printElement(node as ReactElement, path);
  }
  throw new SpecUserError(
    `Invalid \`${path}\`: expected a JSX element or text, got ${describeValue(node)}.`,
  );
}

// Tag and attribute names are interpolated into the generated source without
// escaping, so they are restricted to names JSX could have produced.
const HOST_ELEMENT_TAG_REGEX = /^[a-z][a-z0-9-]*$/;
const ATTRIBUTE_NAME_REGEX = /^[a-zA-Z][a-zA-Z0-9-]*$/;

function printElement(element: ReactElement, path: string): string {
  const { type, props } = element;

  if ((type as unknown) === REACT_FRAGMENT_SYMBOL) {
    const children = (props as { children?: unknown }).children;
    return printNode(children, `${path}.children`) ?? "";
  }

  if (typeof type !== "string") {
    const typeDescription =
      typeof type === "function"
        ? `a component${type.name ? ` (\`${type.name}\`)` : ""}`
        : describeValue(type);
    throw new SpecUserError(
      `Invalid element in \`${path}\`: only plain HTML elements (e.g. \`<meta />\`, \`<link />\`) ` +
        `are supported in \`app.head\`, got ${typeDescription}.`,
    );
  }

  if (!HOST_ELEMENT_TAG_REGEX.test(type)) {
    throw new SpecUserError(
      `Invalid element in \`${path}\`: \`${type}\` is not a valid HTML tag name.`,
    );
  }

  const { children, ...attributes } = props as { children?: unknown } & Record<
    string,
    unknown
  >;

  const printedAttributes = Object.entries(attributes)
    .map(([name, value]) => printAttribute(name, value, path))
    .filter((printedAttribute) => printedAttribute !== null)
    .map((printedAttribute) => ` ${printedAttribute}`)
    .join("");

  const printedChildren = printNode(children, `${path}.children`);
  return printedChildren === null
    ? `<${type}${printedAttributes} />`
    : `<${type}${printedAttributes}>${printedChildren}</${type}>`;
}

function printAttribute(
  name: string,
  value: unknown,
  path: string,
): string | null {
  if (!ATTRIBUTE_NAME_REGEX.test(name)) {
    throw new SpecUserError(
      `Invalid prop \`${name}\` in \`${path}\`: not a valid JSX attribute name.`,
    );
  }
  // Like React, we omit attributes that don't render.
  if (value === false || value === null || value === undefined) {
    return null;
  }
  if (value === true) {
    return name;
  }
  if (
    typeof value === "string" ||
    typeof value === "number" ||
    typeof value === "object"
  ) {
    assertJsonSerializable(value, `${path}.props.${name}`);
    return `${name}={${JSON.stringify(value)}}`;
  }
  throw new SpecUserError(
    `Invalid prop \`${name}\` in \`${path}\`: ${describeValue(value)} is not supported ` +
      `in \`app.head\` elements because they must be serializable.`,
  );
}

function assertJsonSerializable(
  value: unknown,
  path: string,
  seenObjects: WeakSet<object> = new WeakSet(),
): void {
  if (typeof value === "number" && !Number.isFinite(value)) {
    throw new SpecUserError(
      `Invalid value in \`${path}\`: \`${value}\` is not supported ` +
        `in \`app.head\` elements because it must be serializable.`,
    );
  }
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "number" ||
    typeof value === "boolean"
  ) {
    return;
  }
  // `seenObjects` tracks only the current ancestor chain (values are removed
  // again after recursing), so shared non-circular references stay allowed.
  if (typeof value === "object" && seenObjects.has(value)) {
    throw new SpecUserError(
      `Invalid value in \`${path}\`: circular references are not supported ` +
        `in \`app.head\` elements because they must be serializable.`,
    );
  }
  if (Array.isArray(value)) {
    seenObjects.add(value);
    value.forEach((item, index) =>
      assertJsonSerializable(item, `${path}[${index}]`, seenObjects),
    );
    seenObjects.delete(value);
    return;
  }
  if (
    typeof value === "object" &&
    Object.getPrototypeOf(value) === Object.prototype
  ) {
    seenObjects.add(value);
    Object.entries(value).forEach(([key, item]) =>
      assertJsonSerializable(item, `${path}.${key}`, seenObjects),
    );
    seenObjects.delete(value);
    return;
  }
  throw new SpecUserError(
    `Invalid value in \`${path}\`: ${describeValue(value)} is not supported ` +
      `in \`app.head\` elements because it must be serializable.`,
  );
}

function describeValue(value: unknown): string {
  if (typeof value === "string") {
    return `the string ${JSON.stringify(value)}`;
  }
  if (typeof value === "function") {
    return `a function${value.name ? ` (\`${value.name}\`)` : ""}`;
  }
  if (value === null) {
    return "`null`";
  }
  return `a value of type \`${typeof value}\``;
}
