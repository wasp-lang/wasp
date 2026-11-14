export function element<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  properties: Partial<HTMLElementTagNameMap[K]> = {},
  children: (Node | string)[] = [],
): HTMLElementTagNameMap[K] {
  const node = Object.assign(document.createElement(tag), properties);
  node.append(...children);
  return node;
}

export function querySelectorRequired<K extends keyof HTMLElementTagNameMap>(
  root: ParentNode,
  selector: K,
): HTMLElementTagNameMap[K];
export function querySelectorRequired<T extends Element = HTMLElement>(
  root: ParentNode,
  selector: string,
): T;
export function querySelectorRequired(
  root: ParentNode,
  selector: string,
): Element {
  const node = root.querySelector(selector);
  if (node === null) {
    throw new Error(`Missing ${selector} element.`);
  }
  return node;
}

/** Reads `text` out to screen readers. */
export function announce(text: string): void {
  const announcer = querySelectorRequired(document, "#announcer");
  // Clearing first makes repeated texts announce again.
  announcer.textContent = "";
  setTimeout(() => (announcer.textContent = text), 50);
}

export function formatSubject(subject: string): string {
  return subject || "(no subject)";
}
