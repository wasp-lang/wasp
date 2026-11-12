// Runs in the browser. `API_BASE_PATH` is defined by the page.
export const inboxScript = /* JS */ `
  const layout = document.getElementById("layout");
  const emailsList = document.getElementById("emails");
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
    { id: "links", label: (email) => "Links (" + email.links.length + ")" },
    { id: "json", label: () => "JSON" },
  ];

  let allEmails = [];
  let matchingEmails = [];
  let listRequestId = 0;
  let selectedTab = "html";
  let currentEmail;

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

  function selectEmail(id) {
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
      callApi("/emails"),
      searchQuery === "" ? undefined : callApi("/emails?" + searchQuery),
    ]);
    if (requestId !== listRequestId) {
      return;
    }
    allEmails = all?.emails ?? [];
    matchingEmails = searchQuery === "" ? allEmails : (matching?.emails ?? []);
    renderList();
  }

  function renderList() {
    clearButton.disabled = allEmails.length === 0;
    const total = allEmails.length + (allEmails.length === 1 ? " email" : " emails");
    countLabel.textContent =
      matchingEmails === allEmails ? total : matchingEmails.length + " of " + total;

    listEmpty.hidden = matchingEmails.length > 0;
    listEmpty.textContent =
      allEmails.length === 0
        ? "No emails yet. Emails sent with the Dummy email sender show up here."
        : "No emails match your search.";

    const selectedId = getSelectedId();
    emailsList.replaceChildren(
      ...matchingEmails.map((email) => {
        const link = element("a", { href: "#" + encodeURIComponent(email.id) }, [
          element("span", { className: "subject", textContent: email.subject || "(no subject)" }),
          element("span", { className: "meta" }, [
            element("span", { textContent: email.to }),
            element("time", { dateTime: email.sentAt, textContent: formatTime(email.sentAt) }),
          ]),
        ]);
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

  async function renderDetail() {
    const id = getSelectedId();
    if (id === undefined) {
      currentEmail = undefined;
      detail.replaceChildren(element("p", { className: "empty", textContent: "No email selected." }));
      return;
    }
    const data = await callApi("/emails/" + encodeURIComponent(id));
    if (id !== getSelectedId()) {
      return;
    }
    currentEmail = data?.email;
    if (currentEmail === undefined) {
      detail.replaceChildren(
        element("div", { className: "headers back-only" }, [backButton()]),
        element("p", { className: "empty", textContent: "This email no longer exists." }),
      );
      return;
    }

    const email = currentEmail;
    const from = email.from.name ? email.from.name + " <" + email.from.email + ">" : email.from.email;
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
    const tabPanel = element("div", { role: "tabpanel", id: "tabpanel", tabIndex: 0 });

    detail.replaceChildren(
      element("div", { className: "headers" }, [
        backButton(),
        element("h2", { textContent: email.subject || "(no subject)", tabIndex: -1 }),
        element("button", {
          type: "button",
          className: "button secondary delete",
          textContent: "Delete",
          onclick: () => deleteEmail(email.id),
        }),
        element("dl", {}, [
          element("dt", { textContent: "From" }),
          element("dd", { textContent: from }),
          element("dt", { textContent: "To" }),
          element("dd", { textContent: email.to }),
          element("dt", { textContent: "Sent" }),
          element("dd", {}, [
            element("time", { dateTime: email.sentAt, textContent: new Date(email.sentAt).toLocaleString() }),
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

  function backButton() {
    return element("button", {
      type: "button",
      className: "button secondary back",
      textContent: "← Emails",
      onclick: () => selectEmail(undefined),
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
    const email = currentEmail;
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
      iframe.srcdoc = '<base target="_blank">' + email.html;
      tabPanel.replaceChildren(iframe);
    } else if (selectedTab === "text") {
      tabPanel.replaceChildren(element("pre", { textContent: email.text }));
    } else if (selectedTab === "links") {
      tabPanel.replaceChildren(
        email.links.length === 0
          ? element("p", { className: "empty", textContent: "No links in this email." })
          : element(
              "ul",
              { className: "links" },
              email.links.map((link) =>
                element("li", {}, [element("a", { href: link, target: "_blank", rel: "noreferrer", textContent: link })]),
              ),
            ),
      );
    } else {
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
    }
  }

  async function deleteEmail(id) {
    const index = matchingEmails.findIndex((email) => email.id === id);
    const nextEmail = matchingEmails[index + 1] ?? matchingEmails[index - 1];
    await callAction("emails.delete", { id });
    announce("Email deleted.");
    await refreshList();
    selectEmail(nextEmail?.id);
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
      const { email } = JSON.parse(event.data);
      await refreshList();
      announce("New email: " + (email.subject || "(no subject)") + ", to " + email.to + ".");
    });
    for (const type of ["deleted", "cleared"]) {
      events.addEventListener(type, async () => {
        await refreshList();
        if (getSelectedId() !== undefined && currentEmail !== undefined) {
          const isStillThere = allEmails.some((email) => email.id === currentEmail.id);
          if (!isStillThere) {
            renderDetail();
          }
        }
      });
    }
  }

  clearButton.addEventListener("click", async () => {
    if (confirm("Delete all emails?")) {
      await callAction("emails.clear", {});
      announce("All emails deleted.");
      allEmails = [];
      matchingEmails = [];
      selectEmail(undefined);
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
