/** `undefined` deselects the open email. */
export type EmailSelectEvent = CustomEvent<{ id: string | undefined }>;
export type EmailDeleteEvent = CustomEvent<{ id: string }>;

export function dispatchEmailSelect(
  target: EventTarget,
  id: string | undefined,
): void {
  target.dispatchEvent(
    new CustomEvent("email-select", { bubbles: true, detail: { id } }),
  );
}

export function dispatchEmailDelete(target: EventTarget, id: string): void {
  target.dispatchEvent(
    new CustomEvent("email-delete", { bubbles: true, detail: { id } }),
  );
}

declare global {
  interface HTMLElementEventMap {
    "email-select": EmailSelectEvent;
    "email-delete": EmailDeleteEvent;
  }
}
