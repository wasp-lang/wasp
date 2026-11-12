// Wasp brand tokens, kept in sync with wasp.sh (web/tailwind.config.js).
// Small text uses --g6 or darker for a 4.5:1 contrast ratio.
export const inboxStyles = /* CSS */ `
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
  /* Uppercase text has no descenders, so line-height: 1 centers it. */
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

  /* Phone: the list and the email are separate views. */
  @media (max-width: 720px) {
    :root { --gutter: 0.75rem; }
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
