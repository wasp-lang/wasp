/*
  The inbox UI. It's a single self-contained page that uses the same JSON API
  tests and agents use, and listens to the events stream for live updates.

  It follows the wasp.sh style, like the server's root page in development
  (`server/src/views/wrong-port.ts`) and Wasp Studio. The server doesn't serve
  static files, so the styles and the logo are inlined.

  On narrow screens, the list and the selected email are separate views.

  The page renders message content only through `textContent` and the
  sandboxed iframe's `srcdoc`, never through `innerHTML`.
*/

// The /* HTML */ comment is a hint to `prettier` to format this string as HTML.
export const makeInboxPage = ({
  appName,
  apiBasePath,
  directoryPath,
}: {
  appName: string;
  apiBasePath: string;
  /** Shown to the user, so prefer a path relative to the project. */
  directoryPath: string;
}): string => /* HTML */ `
  <!doctype html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>${escapeHtml(appName)} Dev Mailbox</title>
      <style>
        ${styles}
      </style>
    </head>
    <body>
      <header>
        <h1 class="brand">
          ${logo}
          <span class="wordmark">wasp</span>
          <span class="badge">Mailbox</span>
          <span class="separator" aria-hidden="true">/</span>
          <span class="app-name">${escapeHtml(appName)}</span>
        </h1>
        <div class="toolbar">
          <span id="connection" class="connection" role="status">
            <span class="dot" aria-hidden="true"></span>
            <span id="connection-label" class="connection-label"
              >Connecting</span
            >
          </span>
        </div>
      </header>
      <main id="layout" data-view="list">
        <section class="list-panel" aria-labelledby="list-heading">
          <h2 id="list-heading" class="visually-hidden">Emails</h2>
          <div class="list-header">
            <label for="search" class="visually-hidden">Search emails</label>
            <input
              id="search"
              type="search"
              placeholder="Search emails"
              autocomplete="off"
            />
            <div
              id="search-fields"
              class="search-fields"
              role="group"
              aria-label="Search in"
            >
              <button type="button" data-field="from" aria-pressed="true">
                From
              </button>
              <button type="button" data-field="to" aria-pressed="true">
                To
              </button>
              <button type="button" data-field="subject" aria-pressed="true">
                Subject
              </button>
              <button type="button" data-field="body" aria-pressed="true">
                Body
              </button>
            </div>
            <div class="list-actions">
              <span id="count" class="label"></span>
              <button id="clear" class="button secondary small" type="button">
                Clear all
              </button>
            </div>
          </div>
          <ul id="messages" aria-labelledby="list-heading"></ul>
          <p id="list-empty" class="empty" hidden></p>
        </section>
        <section
          id="detail"
          class="detail-panel"
          aria-label="Selected email"
        ></section>
      </main>
      <footer>
        <span> Stored in <code>${escapeHtml(directoryPath)}</code>. </span>
        <span class="footer-links">
          <a href="${escapeHtml(apiBasePath)}/messages">JSON API</a>
          <a href="https://wasp.sh/docs/features/email#dev-mailbox">Docs</a>
        </span>
      </footer>
      <div id="announcer" class="visually-hidden" aria-live="polite"></div>
      <script>
        const API_BASE_PATH = ${toScriptJson(apiBasePath)};
        ${script};
      </script>
    </body>
  </html>
`;

const logo = /* HTML */ `
  <svg class="logo" viewBox="0 0 1520 1520" aria-hidden="true">
    <rect width="1520" height="1520" fill="#f5c842" />
    <g fill="#111">
      <path
        d="m1007 300-156.98 2e-3 0.499 142.51h85.857c0.0239 2e-3 18.763 1.566 30.461 7.9108 13.537 7.3425 14.626 10.763 21.704 20.417 3.0674 4.1835 4.9651 10.687 6.0494 16.123 1.0827 5.4284 1.3754 9.7642 1.3762 9.7762v144.33l2e-3 0.0252s0.583 7.1093 1.796 15.918c1.213 8.809 3.0448 19.305 5.6079 26.205 4.3676 11.759 13.62 26.659 21.633 36.363 9.3866 11.367 22.228 20.124 34.969 27.568 11.294 6.5984 32.628 11.542 33.548 11.754v2.1847c-0.9202 0.21233-22.254 5.1555-33.548 11.754-12.741 7.4439-25.583 16.201-34.97 27.568-8.0134 9.7041-17.266 24.604-21.633 36.363-2.5631 6.9007-4.3948 17.396-5.6078 26.205-1.213 8.809-1.796 15.918-1.796 15.918l-2e-3 0.0271v144.33c-8.2e-4 0.012-0.29348 4.3478-1.3762 9.7762-1.0842 5.4359-2.982 11.94-6.0494 16.123-7.0789 9.6547-8.1671 13.075-21.704 20.418-11.698 6.3448-30.437 7.9107-30.461 7.9107h-85.857l-0.499 142.51h156.99s7.5461-0.1872 16.483-0.629c8.9371-0.4418 19.238-1.1296 24.889-2.1653 7.1809-1.3165 20.102-3.1616 33.53-9.0719 13.329-5.8668 16.299-7.3647 22.443-10.939 11.758-6.8408 24.169-19.146 34.441-32.311s18.389-27.167 21.463-37.524c6.1235-20.632 6.382-47.126 6.382-47.126v-195.36c0-0.0207 0.145-2.5801 0.505-5.7396 0.3614-3.1722 0.956-6.9416 1.7742-9.1512 0.9504-2.5664 2.8911-7.2999 5.3504-11.905 2.4592-4.6049 5.481-9.0841 8.3701-11.115 5.643-3.9674 12.809-9.5822 20.29-10.61 5.9133-0.81252 13.182-1.4397 13.202-1.4417h53.892v-149.83h-53.892c-0.019-2e-3 -7.2884-0.62915-13.202-1.4417-7.4814-1.028-14.648-6.6429-20.29-10.61-2.8891-2.0312-5.9109-6.5104-8.3701-11.115-2.4593-4.6049-4.4-9.3384-5.3504-11.905-0.8182-2.2095-1.4128-5.9789-1.7742-9.1512-0.36-3.1595-0.5038-5.7188-0.505-5.7396v-195.36s-0.2585-26.494-6.382-47.126c-3.0739-10.357-11.191-24.359-21.463-37.524s-22.683-25.47-34.441-32.311c-6.1438-3.5745-9.1141-5.0723-22.443-10.939-13.427-5.9102-26.349-7.7554-33.53-9.0718-5.6503-1.0358-15.952-1.7236-24.889-2.1654-8.9366-0.4418-16.483-0.62892-16.483-0.62892z"
      />
      <rect x="250" y="830" width="500" height="160" />
      <rect x="250" y="530" width="500" height="160" />
    </g>
  </svg>
`;

// Wasp brand tokens, kept in sync with wasp.sh (web/tailwind.config.js).
// Small text uses --g6 at least, which keeps a 4.5:1 contrast ratio.
const styles = /* CSS */ `
  :root {
    --yellow: #f5c842;
    --yellow-light: #fff3cc;
    --yellow-dark: #d4a930;
    --black: #111;
    --cream: #f7f5f0;
    --white: #fafafa;
    --g2: #ddd;
    --g4: #999;
    --g6: #555;
    --green: #22c55e;
    --font-mono:
      "JetBrains Mono", ui-monospace, "SF Mono", Menlo, Consolas,
      "Liberation Mono", monospace;
    --border: 2px solid var(--black);
    --gutter: 1.5rem;
    font-size: 14px;
  }
  * { box-sizing: border-box; }
  body {
    margin: 0;
    height: 100dvh;
    display: flex;
    flex-direction: column;
    background: var(--cream);
    color: var(--black);
    font-family: var(--font-mono);
    line-height: 1.5;
  }
  a { color: var(--black); }
  :focus-visible { outline: 2px solid var(--black); outline-offset: 2px; }
  .visually-hidden {
    position: absolute;
    width: 1px;
    height: 1px;
    margin: -1px;
    overflow: hidden;
    clip-path: inset(50%);
    white-space: nowrap;
  }

  header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
    min-height: 4rem;
    padding: 0.75rem var(--gutter);
    border-bottom: var(--border);
    background: var(--white);
  }
  .brand, .toolbar { display: flex; align-items: center; gap: 0.75rem; }
  .brand { min-width: 0; margin: 0; font-size: inherit; font-weight: inherit; }
  .logo { flex: none; width: 2.25rem; height: 2.25rem; border: var(--border); }
  .wordmark {
    font-size: 1.25rem;
    font-weight: 800;
    letter-spacing: -0.025em;
    line-height: 1;
  }
  /* Uppercase text has no descenders, so with the page line height it looks
     like it sits too high. Boxes around it center it with line-height: 1. */
  .badge,
  .button,
  [role="tab"] {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    line-height: 1;
  }
  .badge {
    flex: none;
    padding: 0.3125rem 0.5rem;
    background: var(--yellow);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }
  .separator { color: var(--g4); }
  .app-name {
    overflow: hidden;
    font-weight: 700;
    white-space: nowrap;
    text-overflow: ellipsis;
  }
  .label, .connection {
    color: var(--g6);
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    line-height: 1;
    white-space: nowrap;
  }
  .connection { display: flex; align-items: center; gap: 0.5rem; }
  .dot {
    width: 0.5rem;
    height: 0.5rem;
    border: 1px solid var(--black);
    background: var(--yellow);
  }
  .connection.live .dot { background: var(--green); }

  .button {
    flex: none;
    min-height: 2rem;
    padding: 0.5rem 0.875rem;
    border: var(--border);
    background: var(--yellow);
    color: var(--black);
    font: inherit;
    font-size: 12px;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
    cursor: pointer;
  }
  .button:hover { background: var(--yellow-dark); }
  .button.secondary { background: var(--white); }
  .button.secondary:hover { background: var(--yellow-light); }
  .button.small { min-height: 1.75rem; padding: 0.375rem 0.625rem; font-size: 11px; }

  main {
    flex: 1;
    display: grid;
    grid-template-columns: minmax(280px, 1fr) 2.5fr;
    gap: var(--gutter);
    min-height: 0;
    padding: var(--gutter);
  }
  .list-panel, .detail-panel {
    display: flex;
    flex-direction: column;
    min-width: 0;
    min-height: 0;
    border: var(--border);
    background: var(--white);
  }
  .detail-panel { box-shadow: 6px 6px 0 var(--yellow); }
  .list-header {
    display: flex;
    flex-direction: column;
    gap: 0.625rem;
    padding: 0.75rem;
    border-bottom: var(--border);
  }
  .list-header input {
    width: 100%;
    padding: 0.375rem 0.5rem;
    border: var(--border);
    background: var(--cream);
    color: var(--black);
    font: inherit;
    font-size: 12px;
  }
  .list-header input::placeholder { color: var(--g6); }
  .search-fields { display: flex; flex-wrap: wrap; gap: 0.375rem; }
  .search-fields button {
    display: inline-flex;
    align-items: center;
    gap: 0.25rem;
    padding: 0.3125rem 0.5rem;
    border: 1.5px solid var(--black);
    background: var(--white);
    color: var(--g6);
    font: inherit;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.05em;
    line-height: 1;
    text-transform: uppercase;
    cursor: pointer;
  }
  .search-fields button::before { content: "+"; content: "+" / ""; }
  .search-fields button[aria-pressed="true"] {
    background: var(--yellow);
    color: var(--black);
  }
  .search-fields button[aria-pressed="true"]::before {
    content: "✓";
    content: "✓" / "";
  }
  .list-actions {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.75rem;
  }
  ul { list-style: none; margin: 0; padding: 0; overflow-y: auto; }
  #messages a {
    display: block;
    padding: 0.625rem 0.875rem;
    border-bottom: 1px solid var(--g2);
    border-left: 4px solid transparent;
    color: inherit;
    text-decoration: none;
  }
  #messages a:focus-visible { outline-offset: -4px; }
  #messages a:hover { background: var(--cream); }
  #messages a[aria-current="true"] {
    background: var(--yellow-light);
    border-left-color: var(--yellow);
  }
  .subject {
    display: block;
    overflow: hidden;
    font-weight: 700;
    white-space: nowrap;
    text-overflow: ellipsis;
  }
  .meta {
    display: flex;
    justify-content: space-between;
    gap: 0.5rem;
    color: var(--g6);
    font-size: 12px;
  }
  .meta span { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .meta time { flex: none; }
  .empty {
    margin: 0;
    padding: 1rem;
    color: var(--g6);
    font-size: 12px;
    letter-spacing: 0.05em;
    text-transform: uppercase;
  }
  .empty::before { content: "// "; content: "// " / ""; }
  .detail-panel > .empty { margin: auto; text-align: center; }

  .headers {
    display: grid;
    grid-template-columns: 1fr auto;
    grid-template-areas: "subject actions" "meta meta";
    align-items: start;
    gap: 0.75rem 1rem;
    padding: 1rem 1.25rem;
    border-bottom: var(--border);
  }
  .headers h2 {
    grid-area: subject;
    margin: 0;
    font-size: 1.125rem;
    overflow-wrap: anywhere;
  }
  .headers h2:focus { outline: none; }
  .back { display: none; grid-area: back; justify-self: start; }
  .headers.back-only { display: none; }
  .delete { grid-area: actions; }
  .headers dl {
    grid-area: meta;
    display: grid;
    grid-template-columns: max-content 1fr;
    align-items: baseline;
    gap: 0.125rem 1rem;
    margin: 0;
    font-size: 12px;
  }
  .headers dt {
    color: var(--g6);
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }
  .headers dd { margin: 0; overflow-wrap: anywhere; }
  [role="tablist"] {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    padding: 0.625rem 1.25rem;
    border-bottom: var(--border);
  }
  [role="tab"] {
    padding: 0.375rem 0.625rem;
    border: 2px solid transparent;
    background: none;
    color: var(--g6);
    font: inherit;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    cursor: pointer;
  }
  [role="tab"]:hover { color: var(--black); }
  [role="tab"][aria-selected="true"] {
    border-color: var(--black);
    background: var(--yellow);
    color: var(--black);
  }
  [role="tabpanel"] { flex: 1; overflow: auto; min-height: 0; }
  iframe {
    display: block;
    width: 100%;
    height: 100%;
    min-height: 16rem;
    border: none;
    background: white;
  }
  pre {
    margin: 0;
    padding: 1.25rem;
    white-space: pre-wrap;
    overflow-wrap: anywhere;
    font-family: var(--font-mono);
    font-size: 12px;
  }
  .links { padding: 0.5rem 1.25rem; }
  .links li {
    padding: 0.5rem 0;
    border-bottom: 1px solid var(--g2);
    font-size: 12px;
    overflow-wrap: anywhere;
  }

  footer {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    justify-content: space-between;
    gap: 0.25rem 1.25rem;
    padding: 0 var(--gutter) 1rem;
    color: var(--g6);
    font-size: 11px;
  }
  footer code { color: var(--black); font-family: var(--font-mono); }
  .footer-links { display: flex; gap: 1.25rem; margin-left: auto; }
  .json-toolbar {
    position: sticky;
    top: 0;
    display: flex;
    justify-content: flex-end;
    padding: 0.75rem 1.25rem 0;
    background: var(--white);
  }
  .json-toolbar + pre { padding-top: 0.5rem; }
  .button:disabled { opacity: 0.4; cursor: not-allowed; }
  .button:disabled:hover { background: var(--white); }

  /* Tablet: a narrower list. */
  @media (max-width: 1024px) {
    :root { --gutter: 1rem; }
    main { grid-template-columns: minmax(240px, 1fr) 1.75fr; }
  }

  /* Phone: the list and the selected email are separate views. */
  @media (max-width: 720px) {
    :root { --gutter: 0.75rem; }
    /* The toolbar moves to its own line when it doesn't fit, e.g. while
       reconnecting or with a long app name. */
    header {
      flex-wrap: wrap;
      row-gap: 0.5rem;
      min-height: 3.5rem;
      padding-block: 0.5rem;
    }
    .toolbar { margin-left: auto; }
    .wordmark { display: none; }
    main { grid-template-columns: 1fr; }
    main[data-view="list"] .detail-panel,
    main[data-view="detail"] .list-panel { display: none; }
    .detail-panel { box-shadow: 4px 4px 0 var(--yellow); }
    .headers {
      grid-template-columns: auto auto;
      grid-template-areas: "back actions" "subject subject" "meta meta";
      justify-content: space-between;
    }
    .back { display: inline-flex; }
    .headers.back-only { display: grid; }
    .delete { justify-self: end; }
    main[data-view="detail"] ~ footer { display: none; }
  }
`;

const script = /* JS */ `
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

  // All emails, for the total count, and the ones the search matches.
  let messages = [];
  let visibleMessages = [];
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

  // Selection lives in the URL hash, so links to an email and the browser's
  // back and forward buttons work.
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
    messages = all?.messages ?? [];
    visibleMessages = searchQuery === "" ? messages : (matching?.messages ?? []);
    renderList();
  }

  function renderList() {
    clearButton.disabled = messages.length === 0;
    const total = messages.length + (messages.length === 1 ? " email" : " emails");
    countLabel.textContent =
      visibleMessages === messages ? total : visibleMessages.length + " of " + total;

    listEmpty.hidden = visibleMessages.length > 0;
    listEmpty.textContent =
      messages.length === 0
        ? "No emails yet. Emails sent with the Dummy email sender show up here."
        : "No emails match your search.";

    const selectedId = getSelectedId();
    messagesList.replaceChildren(
      ...visibleMessages.map((message) => {
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

  // Arrow keys, Home, and End move between tabs, as in the ARIA tabs pattern.
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
      // No allow-scripts: email scripts never run. Links open in a new tab.
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
    // Select the next email in the list, so you can keep going through them.
    const index = visibleMessages.findIndex((message) => message.id === id);
    const nextMessage = visibleMessages[index + 1] ?? visibleMessages[index - 1];
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
          const isStillThere = messages.some((message) => message.id === currentMessage.id);
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
      messages = [];
      visibleMessages = [];
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
      // Searching in no fields would never match, so keep at least one.
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

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/** Serializes a value for inlining into a `<script>` tag. */
function toScriptJson(value: unknown): string {
  return JSON.stringify(value).replace(/</g, "\\u003c");
}
