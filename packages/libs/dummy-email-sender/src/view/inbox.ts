// Runs in the browser. Email content only goes through `textContent` and the
// sandboxed iframe's `srcdoc`, never `innerHTML`.

import "./inbox.css";

import "./components/mailboxEmailDetail.js";
import "./components/mailboxEmailList.js";
import "./components/mailboxSearch.js";
import "./components/mailboxTabs.js";

import {
  DEV_MAILBOX_API_BASE_PATH,
  devMailboxApiPaths,
} from "../api/apiPaths.js";
import { createDevMailboxApiClient } from "../client/devMailboxClient.js";
import type { DevMailboxEvent } from "../core/devMailbox.js";
import type { DevMailboxEmailSummary } from "../core/email.js";
import { announce, formatSubject, querySelectorRequired } from "./dom.js";

// The page is served at the mailbox root, next to the API.
const API_URL = "." + DEV_MAILBOX_API_BASE_PATH;
const devMailbox = createDevMailboxApiClient(API_URL);

const layout = querySelectorRequired(document, "main");
const search = querySelectorRequired(document, "mailbox-search");
const emailList = querySelectorRequired(document, "mailbox-email-list");
const emailDetail = querySelectorRequired(document, "mailbox-email-detail");
const countLabel = querySelectorRequired(document, "#count");
const clearButton = querySelectorRequired<HTMLButtonElement>(
  document,
  "#clear",
);
const connection = querySelectorRequired(document, "#connection");

let allEmails: DevMailboxEmailSummary[] = [];
let matchingEmails: DevMailboxEmailSummary[] = [];
let listRequestId = 0;

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
  void render({ shouldFocusDetail: true });
}

async function render({ shouldFocusDetail = false } = {}): Promise<void> {
  layout.dataset.view = getSelectedId() === undefined ? "list" : "detail";
  renderList();
  await renderDetail();
  if (shouldFocusDetail && !emailDetail.focusSubject()) {
    search.focus();
  }
}

async function refreshList(): Promise<void> {
  const requestId = ++listRequestId;
  const filter = search.filter;
  const [all, matching] = await Promise.all([
    devMailbox.list(),
    filter === undefined ? undefined : devMailbox.list(filter),
  ]);
  if (requestId !== listRequestId) {
    return;
  }
  allEmails = all;
  matchingEmails = matching ?? allEmails;
  renderList();
}

function renderList(): void {
  clearButton.disabled = allEmails.length === 0;
  const total = `${allEmails.length} ${allEmails.length === 1 ? "email" : "emails"}`;
  countLabel.textContent =
    matchingEmails === allEmails
      ? total
      : `${matchingEmails.length} of ${total}`;
  emailList.render({
    emails: matchingEmails,
    isMailboxEmpty: allEmails.length === 0,
    selectedId: getSelectedId(),
  });
}

async function renderDetail(): Promise<void> {
  const id = getSelectedId();
  if (id === undefined) {
    emailDetail.showNoEmail();
    return;
  }
  const email = await devMailbox.get(id);
  if (id !== getSelectedId()) {
    return;
  }
  if (email === undefined) {
    emailDetail.showMissingEmail();
  } else {
    emailDetail.showEmail(email);
  }
}

async function deleteEmail(id: string): Promise<void> {
  const index = matchingEmails.findIndex((email) => email.id === id);
  const nextEmail = matchingEmails[index + 1] ?? matchingEmails[index - 1];
  await devMailbox.delete(id);
  announce("Email deleted.");
  await refreshList();
  selectEmail(nextEmail?.id);
}

async function clearEmails(): Promise<void> {
  if (!confirm("Delete all emails?")) {
    return;
  }
  await devMailbox.clear();
  announce("All emails deleted.");
  allEmails = [];
  matchingEmails = [];
  selectEmail(undefined);
}

function setConnectionStatus(isLive: boolean): void {
  querySelectorRequired(connection, ".connection-label").textContent = isLive
    ? "Live"
    : "Reconnecting";
  connection.classList.toggle("live", isLive);
}

function listenForChanges(): void {
  const events = new EventSource(API_URL + devMailboxApiPaths.events);
  events.addEventListener("open", () => {
    setConnectionStatus(true);
    void refreshList();
  });
  events.addEventListener("error", () => setConnectionStatus(false));
  events.addEventListener("added", async (event) => {
    const { email } = JSON.parse(event.data) as Extract<
      DevMailboxEvent,
      { type: "added" }
    >;
    await refreshList();
    announce(`New email: ${formatSubject(email.subject)}, to ${email.to}.`);
  });
  for (const type of ["deleted", "cleared"] as const) {
    events.addEventListener(type, async () => {
      await refreshList();
      const openEmail = emailDetail.email;
      if (
        openEmail !== undefined &&
        !allEmails.some((email) => email.id === openEmail.id)
      ) {
        await renderDetail();
      }
    });
  }
}

layout.addEventListener("email-select", (event) =>
  selectEmail(event.detail.id),
);
layout.addEventListener(
  "email-delete",
  (event) => void deleteEmail(event.detail.id),
);
search.addEventListener("filter-change", () => void refreshList());
clearButton.addEventListener("click", () => void clearEmails());
window.addEventListener("popstate", () => void render());
window.addEventListener("hashchange", () => void render());

void render();
void refreshList();
listenForChanges();
