/**
 * The ARIA tabs pattern over its `[role="tab"]` children, each pointing at its
 * panel with `aria-controls`. Keeps the selected tab across content changes.
 */
export class MailboxTabs extends HTMLElement {
  #tabs: HTMLElement[] = [];

  connectedCallback(): void {
    if (this.#tabs.length > 0) {
      return;
    }
    this.#tabs = [...this.querySelectorAll<HTMLElement>('[role="tab"]')];
    for (const tab of this.#tabs) {
      tab.addEventListener("click", () => this.#select(tab));
      tab.addEventListener("keydown", (event) => this.#onKeyDown(event, tab));
    }
    this.#select(this.#tabs[0]!);
  }

  #select(selectedTab: HTMLElement): void {
    for (const tab of this.#tabs) {
      const isSelected = tab === selectedTab;
      tab.setAttribute("aria-selected", String(isSelected));
      tab.tabIndex = isSelected ? 0 : -1;
      getPanel(tab).hidden = !isSelected;
    }
  }

  #onKeyDown(event: KeyboardEvent, tab: HTMLElement): void {
    const index = this.#tabs.indexOf(tab);
    const count = this.#tabs.length;
    const nextIndexByKey: Record<string, number> = {
      ArrowRight: (index + 1) % count,
      ArrowLeft: (index - 1 + count) % count,
      Home: 0,
      End: count - 1,
    };
    const nextTab = this.#tabs[nextIndexByKey[event.key] ?? -1];
    if (nextTab === undefined) {
      return;
    }
    event.preventDefault();
    this.#select(nextTab);
    nextTab.focus();
  }
}

function getPanel(tab: HTMLElement): HTMLElement {
  const panelId = tab.getAttribute("aria-controls") ?? "";
  const panel = document.getElementById(panelId);
  if (panel === null) {
    throw new Error(`Missing #${panelId} tab panel.`);
  }
  return panel;
}

customElements.define("mailbox-tabs", MailboxTabs);

declare global {
  interface HTMLElementTagNameMap {
    "mailbox-tabs": MailboxTabs;
  }
}
