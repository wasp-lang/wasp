import type { DevMailboxEvent } from "../core/devMailbox.js";
import type { DevMailboxEmail, DevMailboxEmailSummary } from "../core/email.js";

export type InboxScriptConfig = {
  apiBasePath: string;
  apiPaths: {
    emails: string;
    /** The path of one email, without its id. */
    emailPrefix: string;
    events: string;
    deleteEmail: string;
    clearEmails: string;
  };
};

type TabId = "html" | "text" | "links" | "json";

/**
 * Runs in the browser. The page inlines it with `toString()`, so it must not
 * reference anything outside its own body at runtime.
 */
export function runInboxScript(config: InboxScriptConfig): void {
  const { apiBasePath, apiPaths } = config;

  function getRequiredElement<T extends HTMLElement>(id: string): T {
    const node = document.getElementById(id);
    if (node === null) {
      throw new Error(`Missing #${id} element.`);
    }
    return node as T;
  }

  const layout = getRequiredElement("layout");
  const emailsList = getRequiredElement("emails");
  const listEmpty = getRequiredElement("list-empty");
  const countLabel = getRequiredElement("count");
  const connection = getRequiredElement("connection");
  const connectionLabel = getRequiredElement("connection-label");
  const detail = getRequiredElement("detail");
  const search = getRequiredElement<HTMLInputElement>("search");
  const announcer = getRequiredElement("announcer");
  const clearButton = getRequiredElement<HTMLButtonElement>("clear");
  const searchFieldButtons = [
    ...document.querySelectorAll<HTMLButtonElement>("#search-fields button"),
  ];

  const TABS: { id: TabId; label: (email: DevMailboxEmail) => string }[] = [
    { id: "html", label: () => "HTML" },
    { id: "text", label: () => "Text" },
    { id: "links", label: (email) => `Links (${email.links.length})` },
    { id: "json", label: () => "JSON" },
  ];

  let allEmails: DevMailboxEmailSummary[] = [];
  let matchingEmails: DevMailboxEmailSummary[] = [];
  let listRequestId = 0;
  let selectedTab: TabId = "html";
  let currentEmail: DevMailboxEmail | undefined;

  async function callApi<T>(
    path: string,
    init?: RequestInit,
  ): Promise<T | undefined> {
    const response = await fetch(apiBasePath + path, init);
    return response.ok ? ((await response.json()) as T) : undefined;
  }

  function callAction(path: string, body: unknown): Promise<unknown> {
    return callApi(path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
  }

  function element<K extends keyof HTMLElementTagNameMap>(
    tag: K,
    properties: Partial<HTMLElementTagNameMap[K]> = {},
    children: (Node | string)[] = [],
  ): HTMLElementTagNameMap[K] {
    const node = Object.assign(document.createElement(tag), properties);
    node.append(...children);
    return node;
  }

  function announce(text: string): void {
    announcer.textContent = "";
    setTimeout(() => (announcer.textContent = text), 50);
  }

  // The URL hash holds the selection, so links and back/forward work.
  function getSelectedId(): string | undefined {
    return decodeURIComponent(location.hash.slice(1)) || undefined;
  }

  function selectEmail(id: string | undefined): void {
    const url =
      id === undefined
        ? location.pathname + location.search
        : "#" + encodeURIComponent(id);
    history.pushState(null, "", url);
    void render({ focusDetail: true });
  }

  async function render({ focusDetail = false } = {}): Promise<void> {
    layout.dataset.view = getSelectedId() === undefined ? "list" : "detail";
    renderList();
    await renderDetail();
    if (focusDetail) {
      (detail.querySelector("h2") ?? search).focus();
    }
  }

  function getSearchQuery(): string {
    const params = new URLSearchParams();
    const query = search.value.trim();
    if (query !== "") {
      params.set("search", query);
      params.set(
        "searchIn",
        searchFieldButtons
          .filter((button) => button.getAttribute("aria-pressed") === "true")
          .map((button) => button.dataset.field)
          .join(","),
      );
    }
    return params.toString();
  }

  async function refreshList(): Promise<void> {
    const requestId = ++listRequestId;
    const searchQuery = getSearchQuery();
    type EmailsResponse = { emails: DevMailboxEmailSummary[] };
    const [all, matching] = await Promise.all([
      callApi<EmailsResponse>(apiPaths.emails),
      searchQuery === ""
        ? undefined
        : callApi<EmailsResponse>(apiPaths.emails + "?" + searchQuery),
    ]);
    if (requestId !== listRequestId) {
      return;
    }
    allEmails = all?.emails ?? [];
    matchingEmails = searchQuery === "" ? allEmails : (matching?.emails ?? []);
    renderList();
  }

  function renderList(): void {
    clearButton.disabled = allEmails.length === 0;
    const total = `${allEmails.length} ${allEmails.length === 1 ? "email" : "emails"}`;
    countLabel.textContent =
      matchingEmails === allEmails
        ? total
        : `${matchingEmails.length} of ${total}`;

    listEmpty.hidden = matchingEmails.length > 0;
    listEmpty.textContent =
      allEmails.length === 0
        ? "No emails yet. Emails sent with the Dummy email sender show up here."
        : "No emails match your search.";

    const selectedId = getSelectedId();
    emailsList.replaceChildren(
      ...matchingEmails.map((email) => {
        const link = element(
          "a",
          { href: "#" + encodeURIComponent(email.id) },
          [
            element("span", {
              className: "subject",
              textContent: email.subject || "(no subject)",
            }),
            element("span", { className: "meta" }, [
              element("span", { textContent: email.to }),
              element("time", {
                dateTime: email.sentAt,
                textContent: formatTime(email.sentAt),
              }),
            ]),
          ],
        );
        if (email.id === selectedId) {
          link.setAttribute("aria-current", "true");
        }
        link.addEventListener("click", (event) => {
          event.preventDefault();
          selectEmail(email.id);
        });
        return element("li", {}, [link]);
      }),
    );
  }

  async function renderDetail(): Promise<void> {
    const id = getSelectedId();
    if (id === undefined) {
      currentEmail = undefined;
      detail.replaceChildren(
        element("p", { className: "empty", textContent: "No email selected." }),
      );
      return;
    }
    const response = await callApi<{ email: DevMailboxEmail }>(
      apiPaths.emailPrefix + encodeURIComponent(id),
    );
    if (id !== getSelectedId()) {
      return;
    }
    currentEmail = response?.email;
    if (currentEmail === undefined) {
      detail.replaceChildren(
        element("div", { className: "headers back-only" }, [backButton()]),
        element("p", {
          className: "empty",
          textContent: "This email no longer exists.",
        }),
      );
      return;
    }

    const email = currentEmail;
    const from = email.from.name
      ? `${email.from.name} <${email.from.email}>`
      : email.from.email;
    const tabButtons = TABS.map((tab) => {
      const button = element("button", {
        type: "button",
        role: "tab",
        id: "tab-" + tab.id,
        textContent: tab.label(email),
      });
      button.setAttribute("aria-controls", "tabpanel");
      button.addEventListener("click", () => selectTab(tab.id));
      button.addEventListener("keydown", onTabKeyDown);
      return button;
    });
    const tabList = element("div", { role: "tablist" }, tabButtons);
    tabList.setAttribute("aria-label", "Email views");
    const tabPanel = element("div", {
      role: "tabpanel",
      id: "tabpanel",
      tabIndex: 0,
    });

    detail.replaceChildren(
      element("div", { className: "headers" }, [
        backButton(),
        element("h2", {
          textContent: email.subject || "(no subject)",
          tabIndex: -1,
        }),
        element("button", {
          type: "button",
          className: "button secondary delete",
          textContent: "Delete",
          onclick: () => void deleteEmail(email.id),
        }),
        element("dl", {}, [
          element("dt", { textContent: "From" }),
          element("dd", { textContent: from }),
          element("dt", { textContent: "To" }),
          element("dd", { textContent: email.to }),
          element("dt", { textContent: "Sent" }),
          element("dd", {}, [
            element("time", {
              dateTime: email.sentAt,
              textContent: new Date(email.sentAt).toLocaleString(),
            }),
          ]),
          element("dt", { textContent: "Id" }),
          element("dd", { textContent: email.id }),
        ]),
      ]),
      tabList,
      tabPanel,
    );
    renderTabPanel();
  }

  function backButton(): HTMLButtonElement {
    return element("button", {
      type: "button",
      className: "button secondary back",
      textContent: "← Emails",
      onclick: () => selectEmail(undefined),
    });
  }

  function selectTab(tabId: TabId): void {
    selectedTab = tabId;
    renderTabPanel();
  }

  // ARIA tabs pattern
  function onTabKeyDown(event: KeyboardEvent): void {
    const index = TABS.findIndex((tab) => tab.id === selectedTab);
    const nextIndexByKey: Record<string, number> = {
      ArrowRight: (index + 1) % TABS.length,
      ArrowLeft: (index - 1 + TABS.length) % TABS.length,
      Home: 0,
      End: TABS.length - 1,
    };
    const nextTab = TABS[nextIndexByKey[event.key] ?? -1];
    if (nextTab === undefined) {
      return;
    }
    event.preventDefault();
    selectTab(nextTab.id);
    getRequiredElement("tab-" + nextTab.id).focus();
  }

  function renderTabPanel(): void {
    const email = currentEmail;
    if (email === undefined) {
      return;
    }
    for (const tab of TABS) {
      const button = getRequiredElement("tab-" + tab.id);
      const isSelected = tab.id === selectedTab;
      button.setAttribute("aria-selected", String(isSelected));
      button.tabIndex = isSelected ? 0 : -1;
    }
    const tabPanel = getRequiredElement("tabpanel");
    tabPanel.setAttribute("aria-labelledby", "tab-" + selectedTab);

    switch (selectedTab) {
      case "html": {
        const iframe = element("iframe", { title: "Email HTML body" });
        // Without allow-scripts, email scripts never run.
        iframe.setAttribute(
          "sandbox",
          "allow-popups allow-popups-to-escape-sandbox",
        );
        iframe.srcdoc = '<base target="_blank">' + email.html;
        tabPanel.replaceChildren(iframe);
        return;
      }
      case "text":
        tabPanel.replaceChildren(element("pre", { textContent: email.text }));
        return;
      case "links":
        tabPanel.replaceChildren(
          email.links.length === 0
            ? element("p", {
                className: "empty",
                textContent: "No links in this email.",
              })
            : element(
                "ul",
                { className: "links" },
                email.links.map((link) =>
                  element("li", {}, [
                    element("a", {
                      href: link,
                      target: "_blank",
                      rel: "noreferrer",
                      textContent: link,
                    }),
                  ]),
                ),
              ),
        );
        return;
      case "json": {
        const json = JSON.stringify(email, null, 2);
        const copyButton = element("button", {
          type: "button",
          className: "button secondary small",
          textContent: "Copy JSON",
        });
        copyButton.addEventListener("click", async () => {
          await navigator.clipboard.writeText(json);
          copyButton.textContent = "Copied!";
          announce("JSON copied.");
          setTimeout(() => (copyButton.textContent = "Copy JSON"), 2000);
        });
        tabPanel.replaceChildren(
          element("div", { className: "json-toolbar" }, [copyButton]),
          element("pre", { textContent: json }),
        );
        return;
      }
    }
  }

  async function deleteEmail(id: string): Promise<void> {
    const index = matchingEmails.findIndex((email) => email.id === id);
    const nextEmail = matchingEmails[index + 1] ?? matchingEmails[index - 1];
    await callAction(apiPaths.deleteEmail, { id });
    announce("Email deleted.");
    await refreshList();
    selectEmail(nextEmail?.id);
  }

  function formatTime(isoTimestamp: string): string {
    const date = new Date(isoTimestamp);
    const isToday = date.toDateString() === new Date().toDateString();
    return isToday ? date.toLocaleTimeString() : date.toLocaleDateString();
  }

  function setConnectionStatus(isLive: boolean): void {
    connectionLabel.textContent = isLive ? "Live" : "Reconnecting";
    connection.classList.toggle("live", isLive);
  }

  function listenForChanges(): void {
    const events = new EventSource(apiBasePath + apiPaths.events);
    events.addEventListener("open", () => {
      setConnectionStatus(true);
      void refreshList();
    });
    events.addEventListener("error", () => setConnectionStatus(false));
    events.addEventListener("added", async (event) => {
      const addedEvent = JSON.parse(event.data) as Extract<
        DevMailboxEvent,
        { type: "added" }
      >;
      await refreshList();
      announce(
        `New email: ${addedEvent.email.subject || "(no subject)"}, to ${addedEvent.email.to}.`,
      );
    });
    for (const type of ["deleted", "cleared"] as const) {
      events.addEventListener(type, async () => {
        await refreshList();
        const openEmail = currentEmail;
        if (
          openEmail !== undefined &&
          !allEmails.some((email) => email.id === openEmail.id)
        ) {
          await renderDetail();
        }
      });
    }
  }

  clearButton.addEventListener("click", async () => {
    if (!confirm("Delete all emails?")) {
      return;
    }
    await callAction(apiPaths.clearEmails, {});
    announce("All emails deleted.");
    allEmails = [];
    matchingEmails = [];
    selectEmail(undefined);
  });

  let searchTimeout: ReturnType<typeof setTimeout> | undefined;
  search.addEventListener("input", () => {
    clearTimeout(searchTimeout);
    searchTimeout = setTimeout(() => void refreshList(), 150);
  });
  for (const button of searchFieldButtons) {
    button.addEventListener("click", () => {
      const isPressed = button.getAttribute("aria-pressed") === "true";
      const pressedCount = searchFieldButtons.filter(
        (other) => other.getAttribute("aria-pressed") === "true",
      ).length;
      // Nothing matches with no fields.
      if (isPressed && pressedCount === 1) {
        announce("Search needs at least one field.");
        return;
      }
      button.setAttribute("aria-pressed", String(!isPressed));
      void refreshList();
    });
  }
  window.addEventListener("popstate", () => void render());
  window.addEventListener("hashchange", () => void render());

  void render();
  void refreshList();
  listenForChanges();
}
