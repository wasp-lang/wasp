// Runs in the browser. `API_BASE_PATH` is defined by the page.
export const inboxScript = /* JS */ `
  const layout = document.getElementById("layout");
  const messagesList = document.getElementById("messages");
  const listEmpty = document.getElementById("list-empty");
  const countLabel = document.getElementById("count");
  const connection = document.getElementById("connection");
  const connectionLabel = document.getElementById("connection-label");
  const detail = document.getElementById("detail");
  const search = document.getElementById("search");
  const announcer = document.getElementById("announcer");
  const clearButton = document.getElementById("clear");
  const searchFieldButtons = [...document.querySelectorAll("#search-fields button")];

  const TABS = [
    { id: "html", label: () => "HTML" },
    { id: "text", label: () => "Text" },
    { id: "links", label: (message) => "Links (" + message.links.length + ")" },
    { id: "json", label: () => "JSON" },
  ];

  let allMessages = [];
  let matchingMessages = [];
  let listRequestId = 0;
  let selectedTab = "html";
  let currentMessage;

  async function callApi(path, init) {
    const response = await fetch(API_BASE_PATH + path, init);
    return response.ok ? response.json() : undefined;
  }

  function callAction(action, body) {
    return callApi("/" + action, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
  }

  function element(tag, properties, children) {
    const node = Object.assign(document.createElement(tag), properties);
    for (const child of children ?? []) {
      node.append(child);
    }
    return node;
  }

  function announce(text) {
    announcer.textContent = "";
    setTimeout(() => (announcer.textContent = text), 50);
  }

  // The URL hash holds the selection, so links and back/forward work.
  function getSelectedId() {
    return decodeURIComponent(location.hash.slice(1)) || undefined;
  }

  function selectMessage(id) {
    if (id === undefined) {
      history.pushState(null, "", location.pathname + location.search);
    } else {
      history.pushState(null, "", "#" + encodeURIComponent(id));
    }
    render({ focusDetail: true });
  }

  async function render({ focusDetail = false } = {}) {
    layout.dataset.view = getSelectedId() === undefined ? "list" : "detail";
    renderList();
    await renderDetail();
    if (focusDetail) {
      (detail.querySelector("h2") ?? search).focus();
    }
  }

  function getSearchQuery() {
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

  async function refreshList() {
    const requestId = ++listRequestId;
    const searchQuery = getSearchQuery();
    const [all, matching] = await Promise.all([
      callApi("/messages"),
      searchQuery === "" ? undefined : callApi("/messages?" + searchQuery),
    ]);
    if (requestId !== listRequestId) {
      return;
    }
    allMessages = all?.messages ?? [];
    matchingMessages = searchQuery === "" ? allMessages : (matching?.messages ?? []);
    renderList();
  }

  function renderList() {
    clearButton.disabled = allMessages.length === 0;
    const total = allMessages.length + (allMessages.length === 1 ? " email" : " emails");
    countLabel.textContent =
      matchingMessages === allMessages ? total : matchingMessages.length + " of " + total;

    listEmpty.hidden = matchingMessages.length > 0;
    listEmpty.textContent =
      allMessages.length === 0
        ? "No emails yet. Emails sent with the Dummy email sender show up here."
        : "No emails match your search.";

    const selectedId = getSelectedId();
    messagesList.replaceChildren(
      ...matchingMessages.map((message) => {
        const link = element("a", { href: "#" + encodeURIComponent(message.id) }, [
          element("span", { className: "subject", textContent: message.subject || "(no subject)" }),
          element("span", { className: "meta" }, [
            element("span", { textContent: message.to }),
            element("time", { dateTime: message.sentAt, textContent: formatTime(message.sentAt) }),
          ]),
        ]);
        if (message.id === selectedId) {
          link.setAttribute("aria-current", "true");
        }
        link.addEventListener("click", (event) => {
          event.preventDefault();
          selectMessage(message.id);
        });
        return element("li", {}, [link]);
      }),
    );
  }

  async function renderDetail() {
    const id = getSelectedId();
    if (id === undefined) {
      currentMessage = undefined;
      detail.replaceChildren(element("p", { className: "empty", textContent: "No email selected." }));
      return;
    }
    const data = await callApi("/messages/" + encodeURIComponent(id));
    if (id !== getSelectedId()) {
      return;
    }
    currentMessage = data?.message;
    if (currentMessage === undefined) {
      detail.replaceChildren(
        element("div", { className: "headers back-only" }, [backButton()]),
        element("p", { className: "empty", textContent: "This email no longer exists." }),
      );
      return;
    }

    const message = currentMessage;
    const from = message.from.name ? message.from.name + " <" + message.from.email + ">" : message.from.email;
    const tabButtons = TABS.map((tab) => {
      const button = element("button", {
        type: "button",
        role: "tab",
        id: "tab-" + tab.id,
        textContent: tab.label(message),
      });
      button.setAttribute("aria-controls", "tabpanel");
      button.addEventListener("click", () => selectTab(tab.id));
      button.addEventListener("keydown", onTabKeyDown);
      return button;
    });
    const tabList = element("div", { role: "tablist" }, tabButtons);
    tabList.setAttribute("aria-label", "Email views");
    const tabPanel = element("div", { role: "tabpanel", id: "tabpanel", tabIndex: 0 });

    detail.replaceChildren(
      element("div", { className: "headers" }, [
        backButton(),
        element("h2", { textContent: message.subject || "(no subject)", tabIndex: -1 }),
        element("button", {
          type: "button",
          className: "button secondary delete",
          textContent: "Delete",
          onclick: () => deleteMessage(message.id),
        }),
        element("dl", {}, [
          element("dt", { textContent: "From" }),
          element("dd", { textContent: from }),
          element("dt", { textContent: "To" }),
          element("dd", { textContent: message.to }),
          element("dt", { textContent: "Sent" }),
          element("dd", {}, [
            element("time", { dateTime: message.sentAt, textContent: new Date(message.sentAt).toLocaleString() }),
          ]),
          element("dt", { textContent: "Id" }),
          element("dd", { textContent: message.id }),
        ]),
      ]),
      tabList,
      tabPanel,
    );
    renderTabPanel();
  }

  function backButton() {
    return element("button", {
      type: "button",
      className: "button secondary back",
      textContent: "← Emails",
      onclick: () => selectMessage(undefined),
    });
  }

  function selectTab(tabId) {
    selectedTab = tabId;
    renderTabPanel();
  }

  // ARIA tabs pattern
  function onTabKeyDown(event) {
    const index = TABS.findIndex((tab) => tab.id === selectedTab);
    const nextIndexByKey = {
      ArrowRight: (index + 1) % TABS.length,
      ArrowLeft: (index - 1 + TABS.length) % TABS.length,
      Home: 0,
      End: TABS.length - 1,
    };
    const nextIndex = nextIndexByKey[event.key];
    if (nextIndex === undefined) {
      return;
    }
    event.preventDefault();
    selectTab(TABS[nextIndex].id);
    document.getElementById("tab-" + selectedTab).focus();
  }

  function renderTabPanel() {
    const message = currentMessage;
    for (const tab of TABS) {
      const button = document.getElementById("tab-" + tab.id);
      const isSelected = tab.id === selectedTab;
      button.setAttribute("aria-selected", String(isSelected));
      button.tabIndex = isSelected ? 0 : -1;
    }
    const tabPanel = document.getElementById("tabpanel");
    tabPanel.setAttribute("aria-labelledby", "tab-" + selectedTab);

    if (selectedTab === "html") {
      const iframe = element("iframe", { title: "Email HTML body" });
      // Without allow-scripts, email scripts never run.
      iframe.setAttribute("sandbox", "allow-popups allow-popups-to-escape-sandbox");
      iframe.srcdoc = '<base target="_blank">' + message.html;
      tabPanel.replaceChildren(iframe);
    } else if (selectedTab === "text") {
      tabPanel.replaceChildren(element("pre", { textContent: message.text }));
    } else if (selectedTab === "links") {
      tabPanel.replaceChildren(
        message.links.length === 0
          ? element("p", { className: "empty", textContent: "No links in this email." })
          : element(
              "ul",
              { className: "links" },
              message.links.map((link) =>
                element("li", {}, [element("a", { href: link, target: "_blank", rel: "noreferrer", textContent: link })]),
              ),
            ),
      );
    } else {
      const json = JSON.stringify(message, null, 2);
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
    }
  }

  async function deleteMessage(id) {
    const index = matchingMessages.findIndex((message) => message.id === id);
    const nextMessage = matchingMessages[index + 1] ?? matchingMessages[index - 1];
    await callAction("messages.delete", { id });
    announce("Email deleted.");
    await refreshList();
    selectMessage(nextMessage?.id);
  }

  function formatTime(isoTimestamp) {
    const date = new Date(isoTimestamp);
    const isToday = date.toDateString() === new Date().toDateString();
    return isToday ? date.toLocaleTimeString() : date.toLocaleDateString();
  }

  function setConnectionStatus(isLive) {
    connectionLabel.textContent = isLive ? "Live" : "Reconnecting";
    connection.classList.toggle("live", isLive);
  }

  function listenForChanges() {
    const events = new EventSource(API_BASE_PATH + "/events");
    events.addEventListener("open", () => {
      setConnectionStatus(true);
      refreshList();
    });
    events.addEventListener("error", () => setConnectionStatus(false));
    events.addEventListener("added", async (event) => {
      const { message } = JSON.parse(event.data);
      await refreshList();
      announce("New email: " + (message.subject || "(no subject)") + ", to " + message.to + ".");
    });
    for (const type of ["deleted", "cleared"]) {
      events.addEventListener(type, async () => {
        await refreshList();
        if (getSelectedId() !== undefined && currentMessage !== undefined) {
          const isStillThere = allMessages.some((message) => message.id === currentMessage.id);
          if (!isStillThere) {
            renderDetail();
          }
        }
      });
    }
  }

  clearButton.addEventListener("click", async () => {
    if (confirm("Delete all emails?")) {
      await callAction("messages.clear", {});
      announce("All emails deleted.");
      allMessages = [];
      matchingMessages = [];
      selectMessage(undefined);
    }
  });

  let searchTimeout;
  search.addEventListener("input", () => {
    clearTimeout(searchTimeout);
    searchTimeout = setTimeout(refreshList, 150);
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
      refreshList();
    });
  }
  window.addEventListener("popstate", () => render());
  window.addEventListener("hashchange", () => render());

  render();
  refreshList();
  listenForChanges();
`;
