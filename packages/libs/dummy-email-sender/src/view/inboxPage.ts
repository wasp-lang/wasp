import { inboxScript } from "./inboxScript.js";
import { inboxStyles } from "./inboxStyles.js";
import { waspLogo } from "./waspLogo.js";

/*
  Self-contained, since the server doesn't serve static files, and built on
  the same JSON API tests and agents use. Styled like wasp.sh and Wasp Studio.

  Email content only goes through `textContent` and the sandboxed iframe's
  `srcdoc`, never `innerHTML`.
*/

// The /* HTML */ comment is a hint to `prettier` to format this string as HTML.
export const makeInboxPage = ({
  appName,
  apiBasePath,
  directoryPath,
}: {
  appName: string;
  apiBasePath: string;
  directoryPath: string;
}): string => /* HTML */ `
  <!doctype html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>${escapeHtml(appName)} Dev Mailbox</title>
      <style>
        ${inboxStyles}
      </style>
    </head>
    <body>
      <header>
        <h1 class="brand">
          ${waspLogo}
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
          <ul id="emails" aria-labelledby="list-heading"></ul>
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
          <a href="${escapeHtml(apiBasePath)}/emails">JSON API</a>
          <a href="https://wasp.sh/docs/features/email#dev-mailbox">Docs</a>
        </span>
      </footer>
      <div id="announcer" class="visually-hidden" aria-live="polite"></div>
      <script>
        const API_BASE_PATH = ${toInlineScriptJson(apiBasePath)};
        ${inboxScript};
      </script>
    </body>
  </html>
`;

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function toInlineScriptJson(value: unknown): string {
  return JSON.stringify(value).replace(/</g, "\\u003c");
}
