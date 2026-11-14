import type { DevMailboxEmailSummary } from "../../core/email.js";
import { element, formatSubject, querySelectorRequired } from "../dom.js";
import { dispatchEmailSelect } from "../events.js";
/** Lists email summaries. Fires `email-select` when one is picked. */
export class MailboxEmailList extends HTMLElement {
  render({
    emails,
    isMailboxEmpty,
    selectedId,
  }: {
    emails: DevMailboxEmailSummary[];
    isMailboxEmpty: boolean;
    selectedId: string | undefined;
  }): void {
    const emptyMessage = querySelectorRequired(this, ".empty");
    emptyMessage.hidden = emails.length > 0;
    emptyMessage.textContent = isMailboxEmpty
      ? "No emails yet. Emails sent with the Dummy email sender show up here."
      : "No emails match your search.";

    querySelectorRequired(this, "ul").replaceChildren(
      ...emails.map((email) =>
        element("li", {}, [this.#makeLink(email, email.id === selectedId)]),
      ),
    );
  }

  #makeLink(
    email: DevMailboxEmailSummary,
    isSelected: boolean,
  ): HTMLAnchorElement {
    const link = element("a", { href: "#" + encodeURIComponent(email.id) }, [
      element("span", {
        className: "subject",
        textContent: formatSubject(email.subject),
      }),
      element("span", { className: "meta" }, [
        element("span", { textContent: email.to }),
        element("time", {
          dateTime: email.sentAt,
          textContent: formatTime(email.sentAt),
        }),
      ]),
    ]);
    if (isSelected) {
      link.setAttribute("aria-current", "true");
    }
    link.addEventListener("click", (event) => {
      event.preventDefault();
      dispatchEmailSelect(this, email.id);
    });
    return link;
  }
}

function formatTime(isoTimestamp: string): string {
  const date = new Date(isoTimestamp);
  const isToday = date.toDateString() === new Date().toDateString();
  return isToday ? date.toLocaleTimeString() : date.toLocaleDateString();
}

customElements.define("mailbox-email-list", MailboxEmailList);

declare global {
  interface HTMLElementTagNameMap {
    "mailbox-email-list": MailboxEmailList;
  }
}
