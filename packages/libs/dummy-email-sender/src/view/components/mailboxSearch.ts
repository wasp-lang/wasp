import {
  DEV_MAILBOX_SEARCH_FIELDS,
  type DevMailboxEmailFilter,
  type DevMailboxSearchField,
} from "../../core/emailFilter.js";
import { announce, element, querySelectorRequired } from "../dom.js";
const SEARCH_FIELD_LABELS: Record<DevMailboxSearchField, string> = {
  from: "From",
  to: "To",
  subject: "Subject",
  body: "Body",
};

const TYPING_DEBOUNCE_MS = 150;

/**
 * The search input and its field toggles. Fires `filter-change` when the
 * filter changes.
 */
export class MailboxSearch extends HTMLElement {
  #input!: HTMLInputElement;
  #fieldButtons: HTMLButtonElement[] = [];

  connectedCallback(): void {
    if (this.#fieldButtons.length > 0) {
      return;
    }
    this.#input = querySelectorRequired<HTMLInputElement>(this, "input");
    this.#fieldButtons = DEV_MAILBOX_SEARCH_FIELDS.map((field) =>
      this.#makeFieldButton(field),
    );
    querySelectorRequired(this, ".search-fields").replaceChildren(
      ...this.#fieldButtons,
    );

    let typingTimeout: ReturnType<typeof setTimeout> | undefined;
    this.#input.addEventListener("input", () => {
      clearTimeout(typingTimeout);
      typingTimeout = setTimeout(
        () => this.#dispatchFilterChange(),
        TYPING_DEBOUNCE_MS,
      );
    });
  }

  /** `undefined` when there is nothing to search for. */
  get filter(): DevMailboxEmailFilter | undefined {
    const query = this.#input.value.trim();
    if (query === "") {
      return undefined;
    }
    return {
      search: query,
      searchIn: DEV_MAILBOX_SEARCH_FIELDS.filter((_field, index) =>
        isPressed(this.#fieldButtons[index]!),
      ),
    };
  }

  override focus(options?: FocusOptions): void {
    this.#input.focus(options);
  }

  #makeFieldButton(field: DevMailboxSearchField): HTMLButtonElement {
    const button = element("button", {
      type: "button",
      textContent: SEARCH_FIELD_LABELS[field],
    });
    button.setAttribute("aria-pressed", "true");
    button.addEventListener("click", () => this.#toggleField(button));
    return button;
  }

  #toggleField(button: HTMLButtonElement): void {
    const pressedCount = this.#fieldButtons.filter(isPressed).length;
    // Nothing matches with no fields.
    if (isPressed(button) && pressedCount === 1) {
      announce("Search needs at least one field.");
      return;
    }
    button.setAttribute("aria-pressed", String(!isPressed(button)));
    this.#dispatchFilterChange();
  }

  #dispatchFilterChange(): void {
    this.dispatchEvent(new Event("filter-change"));
  }
}

function isPressed(button: HTMLButtonElement): boolean {
  return button.getAttribute("aria-pressed") === "true";
}

customElements.define("mailbox-search", MailboxSearch);

declare global {
  interface HTMLElementTagNameMap {
    "mailbox-search": MailboxSearch;
  }
  interface HTMLElementEventMap {
    "filter-change": Event;
  }
}
