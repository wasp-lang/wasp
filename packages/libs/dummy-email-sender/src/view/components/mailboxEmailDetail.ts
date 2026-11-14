import type { DevMailboxEmail } from "../../core/email.js";
import { announce, formatSubject, querySelectorRequired } from "../dom.js";
import { dispatchEmailDelete, dispatchEmailSelect } from "../events.js";
/** Which of the `[data-shown-when]` children are visible. */
type DetailState = "none" | "missing" | "email";

const COPY_JSON_LABEL = "Copy JSON";
const COPIED_LABEL_MS = 2000;

/**
 * Shows the open email, "no email selected", or "email missing". Fires
 * `email-select` with no id to go back to the list, and `email-delete`.
 */
export class MailboxEmailDetail extends HTMLElement {
  #email: DevMailboxEmail | undefined;
  #isInitialized = false;

  connectedCallback(): void {
    if (this.#isInitialized) {
      return;
    }
    this.#isInitialized = true;
    for (const backButton of this.querySelectorAll(".back")) {
      backButton.addEventListener("click", () =>
        dispatchEmailSelect(this, undefined),
      );
    }
    this.#onClick(".delete", () => {
      if (this.#email !== undefined) {
        dispatchEmailDelete(this, this.#email.id);
      }
    });
    this.#onClick(".copy-json", (button) => void this.#copyJson(button));
  }

  /** The open email, `undefined` in the other states. */
  get email(): DevMailboxEmail | undefined {
    return this.#email;
  }

  showNoEmail(): void {
    this.#email = undefined;
    this.#show("none");
  }

  showMissingEmail(): void {
    this.#email = undefined;
    this.#show("missing");
  }

  showEmail(email: DevMailboxEmail): void {
    this.#email = email;
    this.#setText("subject", formatSubject(email.subject));
    this.#setText(
      "from",
      email.from.name
        ? `${email.from.name} <${email.from.email}>`
        : email.from.email,
    );
    this.#setText("to", email.to);
    this.#setText("id", email.id);
    const sentAt = querySelectorRequired<HTMLTimeElement>(this, "time");
    sentAt.dateTime = email.sentAt;
    sentAt.textContent = new Date(email.sentAt).toLocaleString();

    querySelectorRequired<HTMLIFrameElement>(this, "iframe").srcdoc =
      '<base target="_blank">' + email.html;
    this.#setText("html", email.html);
    this.#setText("text", email.text);
    this.#setText("json", JSON.stringify(email, null, 2));
    this.#show("email");
  }

  /** Moves focus to the email's subject. Returns whether there is one. */
  focusSubject(): boolean {
    if (this.#email === undefined) {
      return false;
    }
    querySelectorRequired(this, '[data-email-field="subject"]').focus();
    return true;
  }

  #show(state: DetailState): void {
    for (const child of this.querySelectorAll<HTMLElement>(
      "[data-shown-when]",
    )) {
      child.hidden = child.dataset.shownWhen !== state;
    }
  }

  #setText(field: string, text: string): void {
    querySelectorRequired(this, `[data-email-field="${field}"]`).textContent =
      text;
  }

  #onClick(
    selector: string,
    listener: (button: HTMLButtonElement) => void,
  ): void {
    const button = querySelectorRequired<HTMLButtonElement>(this, selector);
    button.addEventListener("click", () => listener(button));
  }

  async #copyJson(button: HTMLButtonElement): Promise<void> {
    await navigator.clipboard.writeText(JSON.stringify(this.#email, null, 2));
    button.textContent = "Copied!";
    announce("JSON copied.");
    setTimeout(() => (button.textContent = COPY_JSON_LABEL), COPIED_LABEL_MS);
  }
}

customElements.define("mailbox-email-detail", MailboxEmailDetail);

declare global {
  interface HTMLElementTagNameMap {
    "mailbox-email-detail": MailboxEmailDetail;
  }
}
