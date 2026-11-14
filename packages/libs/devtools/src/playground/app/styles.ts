// Follows the look of the Wasp DevTools toolbar and Wasp's starters: monospace
// type, hard black borders, and yellow offset shadows.
export const playgroundStyles = /* css */ `
:root {
  --yellow: #f5c842;
  --yellow-soft: #fdf3cf;
  --black: #111;
  --cream: #f7f5f0;
  --paper: #fffdf8;
  --white: #fff;
  --gray: #6b6b6b;
  --line: #d9d4c7;
  --border: 2px solid var(--black);
  --shadow: 4px 4px 0 var(--black);
  --shadow-yellow: 4px 4px 0 var(--yellow);
  --font-mono: ui-monospace, "SF Mono", "JetBrains Mono", Menlo, Consolas, monospace;
  --font-sans: ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif;

  --green: #1f9d55;
  --green-soft: #dcf5e5;
  --blue: #2563eb;
  --blue-soft: #dde8ff;
  --orange: #d9640b;
  --orange-soft: #ffe8d4;
  --purple: #7c3aed;
  --purple-soft: #ece3ff;
  --red: #dc2626;
  --red-soft: #fde2e2;
  --teal: #0d9488;
  --teal-soft: #d5f5f1;
}

* { box-sizing: border-box; }

html, body {
  margin: 0;
  height: 100%;
  background: var(--cream);
  color: var(--black);
  font-family: var(--font-sans);
  font-size: 13px;
  line-height: 1.45;
  -webkit-font-smoothing: antialiased;
}

#app { height: 100%; }

button { font: inherit; color: inherit; cursor: pointer; }
input, select, textarea { font: inherit; color: inherit; }
code, kbd, .mono { font-family: var(--font-mono); }
code { background: var(--yellow-soft); padding: 0 4px; border: 1px solid var(--line); font-size: 12px; }
kbd {
  display: inline-block;
  min-width: 18px;
  padding: 0 4px;
  border: 1.5px solid var(--black);
  border-bottom-width: 3px;
  background: var(--white);
  font-size: 11px;
  text-align: center;
  line-height: 16px;
}
mark { background: var(--yellow); padding: 0 6px; box-shadow: 3px 3px 0 var(--black); }
.muted { color: var(--gray); }
.spacer { flex: 1; }
.pad { padding: 12px 16px; }

/* Layout */

.app { display: flex; flex-direction: column; height: 100%; }
.layout { flex: 1; display: grid; grid-template-columns: 280px 1fr; min-height: 0; }
.main { min-width: 0; min-height: 0; overflow: hidden; display: flex; flex-direction: column; }

/* Header */

.header {
  display: flex;
  align-items: center;
  gap: 20px;
  padding: 0 16px;
  height: 56px;
  border-bottom: var(--border);
  background: var(--white);
  flex-shrink: 0;
}
.header-brand { display: flex; align-items: center; gap: 10px; }
.header-logo {
  display: grid;
  place-items: center;
  width: 32px;
  height: 32px;
  border: var(--border);
  background: var(--yellow);
  box-shadow: 2px 2px 0 var(--black);
  font-weight: 900;
  font-size: 16px;
}
.header-title { font-family: var(--font-mono); font-weight: 800; font-size: 14px; letter-spacing: -0.02em; }
.header-subtitle { font-family: var(--font-mono); font-size: 11px; color: var(--gray); }
.header-tabs { display: flex; gap: 4px; align-self: stretch; }
.header-tab {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 0 14px;
  border: none;
  background: none;
  font-family: var(--font-mono);
  font-weight: 700;
  border-bottom: 3px solid transparent;
  margin-bottom: -2px;
  color: var(--gray);
}
.header-tab:hover { color: var(--black); }
.header-tab.active { color: var(--black); border-bottom-color: var(--yellow); background: linear-gradient(transparent 70%, var(--yellow-soft)); }
.header-right { margin-left: auto; display: flex; align-items: center; gap: 10px; }
.count-badge {
  background: var(--black);
  color: var(--yellow);
  font-size: 10px;
  padding: 1px 6px;
  border-radius: 10px;
}
.live-dot { width: 8px; height: 8px; border-radius: 50%; background: var(--line); }
.live-dot.on { background: var(--red); animation: pulse 1.6s infinite; }
@keyframes pulse {
  0% { box-shadow: 0 0 0 0 rgba(220, 38, 38, 0.5); }
  70% { box-shadow: 0 0 0 7px rgba(220, 38, 38, 0); }
  100% { box-shadow: 0 0 0 0 rgba(220, 38, 38, 0); }
}
.server-chip, .session-chip {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 4px 10px;
  border: 1.5px solid var(--black);
  background: var(--paper);
  font-family: var(--font-mono);
  font-size: 11px;
}
.session-chip.signed-in { background: var(--green-soft); }
.session-avatar {
  display: grid;
  place-items: center;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  background: var(--black);
  color: var(--yellow);
  font-size: 10px;
  font-weight: 800;
}
.status-dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; background: var(--line); flex-shrink: 0; }
.status-dot.ok, .status-dot.status-ok { background: var(--green); }
.status-dot.bad, .status-dot.status-error { background: var(--red); }
.status-dot.status-warn { background: var(--orange); }
.status-dot.status-redirect { background: var(--blue); }

/* Sidebar */

.sidebar {
  display: flex;
  flex-direction: column;
  border-right: var(--border);
  background: var(--paper);
  min-height: 0;
}
.search {
  display: flex;
  align-items: center;
  gap: 6px;
  margin: 12px;
  padding: 0 8px;
  border: var(--border);
  background: var(--white);
  box-shadow: var(--shadow-yellow);
}
.search input { flex: 1; border: none; outline: none; background: none; padding: 8px 0; font-family: var(--font-mono); }
.search-icon { font-size: 16px; color: var(--gray); }
.sidebar-list { flex: 1; overflow-y: auto; padding: 0 8px 16px; }
.sidebar-group { margin-top: 8px; }
.sidebar-heading {
  margin: 10px 8px 6px;
  font-family: var(--font-mono);
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.12em;
}
.sidebar-item {
  display: flex;
  align-items: center;
  gap: 8px;
  width: 100%;
  padding: 6px 8px;
  border: 2px solid transparent;
  background: none;
  text-align: left;
}
.sidebar-item:hover { background: var(--yellow-soft); }
.sidebar-item.active { border-color: var(--black); background: var(--yellow); box-shadow: 3px 3px 0 var(--black); transform: translate(-1px, -1px); }
.sidebar-item-text { display: flex; flex-direction: column; min-width: 0; flex: 1; }
.sidebar-item-name { font-family: var(--font-mono); font-weight: 700; font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.sidebar-item-meta { font-size: 11px; color: var(--gray); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-family: var(--font-mono); }
.sidebar-item.active .sidebar-item-meta { color: #4a3d0c; }
.sidebar-stat { display: flex; align-items: center; gap: 4px; font-family: var(--font-mono); font-size: 10px; color: var(--gray); }
.lock { margin-left: 4px; font-size: 10px; }
.empty { padding: 16px; text-align: center; }

/* Badges */

.badge {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 54px;
  padding: 2px 5px;
  border: 1.5px solid var(--black);
  font-family: var(--font-mono);
  font-size: 9.5px;
  font-weight: 800;
  letter-spacing: 0.04em;
  flex-shrink: 0;
}
.badge-small { min-width: 46px; font-size: 9px; padding: 1px 4px; }
.kind-query { background: var(--green-soft); color: var(--green); border-color: var(--green); }
.kind-action { background: var(--orange-soft); color: var(--orange); border-color: var(--orange); }
.kind-job { background: var(--purple-soft); color: var(--purple); border-color: var(--purple); }
.method-get { background: var(--blue-soft); color: var(--blue); border-color: var(--blue); }
.method-post { background: var(--teal-soft); color: var(--teal); border-color: var(--teal); }
.method-put, .method-patch { background: var(--orange-soft); color: var(--orange); border-color: var(--orange); }
.method-delete { background: var(--red-soft); color: var(--red); border-color: var(--red); }
.method-all { background: var(--yellow-soft); color: #8a6d00; border-color: #8a6d00; }

.chip {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  padding: 1px 8px;
  border: 1.5px solid var(--black);
  background: var(--white);
  font-family: var(--font-mono);
  font-size: 11px;
}
.chip-entity { background: var(--cream); }
.chip-auth { background: var(--yellow-soft); }

/* Buttons and inputs */

.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
  padding: 7px 14px;
  border: var(--border);
  background: var(--white);
  font-family: var(--font-mono);
  font-weight: 700;
  transition: transform 0.06s, box-shadow 0.06s;
}
.btn:disabled { opacity: 0.5; cursor: default; }
.btn-primary { background: var(--yellow); box-shadow: 3px 3px 0 var(--black); }
.btn-primary:hover:not(:disabled) { transform: translate(-1px, -1px); box-shadow: 4px 4px 0 var(--black); }
.btn-primary:active:not(:disabled) { transform: translate(2px, 2px); box-shadow: 1px 1px 0 var(--black); }
.btn-ghost { border-color: transparent; background: none; padding: 5px 10px; }
.btn-ghost:hover:not(:disabled) { border-color: var(--black); background: var(--white); }
.btn-small { padding: 3px 10px; font-size: 11px; }
.btn-icon { padding: 2px 8px; }
.btn-block { width: 100%; }
.input {
  width: 100%;
  padding: 7px 10px;
  border: 1.5px solid var(--black);
  background: var(--white);
  outline: none;
}
.input:focus { box-shadow: 3px 3px 0 var(--yellow); }

/* Welcome */

.welcome { padding: 48px; max-width: 760px; overflow-y: auto; }
.welcome h1 { font-family: var(--font-mono); font-size: 30px; letter-spacing: -0.03em; margin: 0 0 12px; }
.welcome > p { font-size: 14px; max-width: 560px; }
.welcome-stats { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; margin: 28px 0; }
.stat-card { border: var(--border); background: var(--white); padding: 14px 16px; box-shadow: var(--shadow); }
.stat-number { font-family: var(--font-mono); font-size: 32px; font-weight: 800; line-height: 1; }
.stat-label { font-family: var(--font-mono); font-size: 11px; text-transform: uppercase; letter-spacing: 0.1em; margin-top: 6px; color: var(--gray); }
.kind-border-query { border-top: 6px solid var(--green); }
.kind-border-action { border-top: 6px solid var(--orange); }
.kind-border-api { border-top: 6px solid var(--blue); }
.kind-border-job { border-top: 6px solid var(--purple); }
.welcome-quick { display: flex; flex-wrap: wrap; gap: 8px; }
.quick-item { display: flex; align-items: center; gap: 8px; padding: 6px 10px; border: 1.5px solid var(--black); background: var(--white); font-family: var(--font-mono); font-weight: 700; font-size: 12px; }
.quick-item:hover { background: var(--yellow); }
.hint { margin-top: 28px; }

/* Endpoint */

.endpoint { display: flex; flex-direction: column; height: 100%; min-height: 0; }
.endpoint-header { padding: 16px 20px 14px; border-bottom: var(--border); background: var(--white); }
.endpoint-title-row { display: flex; align-items: center; flex-wrap: wrap; gap: 8px; margin-bottom: 12px; }
.endpoint-title { font-family: var(--font-mono); font-size: 20px; margin: 0 6px 0 2px; letter-spacing: -0.02em; }
.endpoint-stats { margin-left: auto; font-family: var(--font-mono); font-size: 11px; color: var(--gray); }
.endpoint-stats b { color: var(--black); }
.url-bar {
  display: flex;
  align-items: stretch;
  border: var(--border);
  background: var(--white);
  box-shadow: var(--shadow-yellow);
}
.url-method {
  display: flex;
  align-items: center;
  padding: 0 14px;
  font-family: var(--font-mono);
  font-weight: 800;
  border: none;
  border-right: var(--border);
}
.url-text {
  flex: 1;
  display: flex;
  align-items: center;
  padding: 10px 12px;
  font-family: var(--font-mono);
  font-size: 13px;
  overflow-x: auto;
  white-space: nowrap;
}
.url-param { color: var(--purple); background: var(--purple-soft); padding: 0 2px; }
.btn-send { border: none; border-left: var(--border); box-shadow: none; padding: 0 22px; min-width: 130px; }
.btn-send:hover:not(:disabled) { transform: none; box-shadow: none; background: #ffd75e; }
.btn-send:active:not(:disabled) { transform: none; box-shadow: none; background: #e6b92f; }
.kbd-hint { font-size: 10px; opacity: 0.6; font-weight: 400; }
.inline-error { margin-top: 10px; padding: 6px 10px; border: 1.5px solid var(--red); background: var(--red-soft); color: var(--red); font-family: var(--font-mono); font-size: 12px; }

.split { flex: 1; display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); min-height: 0; }
@media (max-width: 1000px) { .split { grid-template-columns: 1fr; grid-template-rows: 1fr 1fr; } }
.pane { display: flex; flex-direction: column; min-height: 0; min-width: 0; }
.request-pane { border-right: var(--border); background: var(--paper); }
@media (max-width: 1000px) { .request-pane { border-right: none; border-bottom: var(--border); } }
.response-pane { background: var(--white); overflow-y: auto; }
.pane-body { flex: 1; overflow: auto; padding: 14px 16px; min-height: 0; }

.tabs {
  display: flex;
  align-items: center;
  gap: 2px;
  padding: 0 10px;
  border-bottom: 1.5px solid var(--line);
  flex-shrink: 0;
}
.tab {
  position: relative;
  padding: 9px 10px;
  border: none;
  background: none;
  font-family: var(--font-mono);
  font-size: 12px;
  font-weight: 700;
  color: var(--gray);
  border-bottom: 3px solid transparent;
  margin-bottom: -1.5px;
}
.tab:hover { color: var(--black); }
.tab.active { color: var(--black); border-bottom-color: var(--black); }
.tab-dot { position: absolute; top: 7px; right: 2px; width: 6px; height: 6px; border-radius: 50%; background: var(--red); }

/* JSON editor */

.body-editor { display: flex; flex-direction: column; height: 100%; min-height: 220px; }
.code-editor {
  flex: 1;
  display: flex;
  border: var(--border);
  background: var(--white);
  overflow: auto;
  min-height: 180px;
}
.code-editor:focus-within { box-shadow: var(--shadow-yellow); }
.code-gutter {
  padding: 10px 8px;
  text-align: right;
  font-family: var(--font-mono);
  font-size: 12.5px;
  line-height: 20px;
  color: #b3ad9f;
  background: var(--cream);
  border-right: 1.5px solid var(--line);
  user-select: none;
  min-width: 36px;
}
.code-area { position: relative; flex: 1; min-width: 0; }
.code-highlight, .code-input {
  margin: 0;
  padding: 10px 12px;
  font-family: var(--font-mono);
  font-size: 12.5px;
  line-height: 20px;
  white-space: pre;
  tab-size: 2;
  border: none;
  overflow: hidden;
}
.code-highlight { pointer-events: none; min-height: 100%; }
.code-input {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  resize: none;
  outline: none;
  background: transparent;
  color: transparent;
  caret-color: var(--black);
}
.code-input::placeholder { color: #b3ad9f; }
.code-input::selection { background: rgba(245, 200, 66, 0.45); color: transparent; }
.editor-footer { display: flex; align-items: center; gap: 10px; padding-top: 8px; font-family: var(--font-mono); font-size: 11px; }
.text-ok { color: var(--green); font-weight: 700; }
.text-error { color: var(--red); font-weight: 700; }

.tok-key { color: #7c3aed; }
.tok-string { color: #157a3c; }
.tok-number { color: #c2410c; }
.tok-boolean { color: #1d4ed8; font-weight: 700; }
.tok-null { color: #9a8f7a; font-style: italic; }
.tok-punct { color: #7a7465; }

/* Forms */

.form-stack { display: flex; flex-direction: column; gap: 12px; }
.form-heading { font-family: var(--font-mono); font-size: 10px; text-transform: uppercase; letter-spacing: 0.12em; margin: 14px 0 6px; color: var(--gray); }
.form-stack > :first-child .form-heading, .form-stack > .field:first-child .form-heading { margin-top: 0; }
.field { display: flex; flex-direction: column; gap: 6px; }
.kv-table { display: flex; flex-direction: column; gap: 6px; }
.kv-row { display: flex; align-items: center; gap: 6px; margin-bottom: 6px; }
.kv-row.disabled .input { opacity: 0.45; }
.kv-key-static { min-width: 110px; font-family: var(--font-mono); font-weight: 700; color: var(--purple); }
.radio-card {
  display: flex;
  gap: 10px;
  align-items: flex-start;
  padding: 10px 12px;
  border: 1.5px solid var(--black);
  background: var(--white);
  cursor: pointer;
}
.radio-card.active { background: var(--yellow-soft); box-shadow: 3px 3px 0 var(--black); }
.radio-card input { margin-top: 3px; accent-color: var(--black); }

.segmented { display: flex; align-items: center; gap: 0; }
.segmented > button:not(.btn) {
  padding: 5px 12px;
  border: 1.5px solid var(--black);
  margin-right: -1.5px;
  background: var(--white);
  font-family: var(--font-mono);
  font-size: 11px;
  font-weight: 700;
}
.segmented > button.active { background: var(--black); color: var(--yellow); }
.code-tab { display: flex; flex-direction: column; gap: 10px; }
.code-block {
  margin: 0;
  padding: 14px;
  border: var(--border);
  background: #16150f;
  color: #f3eedf;
  font-family: var(--font-mono);
  font-size: 12px;
  line-height: 1.6;
  overflow: auto;
  white-space: pre;
}
.code-block .tok-key { color: #c4a7ff; }
.code-block .tok-string { color: #9be3a8; }
.code-block .tok-number { color: #ffb86b; }
.code-block .tok-boolean { color: #8ab4ff; }
.code-block .tok-null { color: #8f8a7c; }
.code-block .tok-punct { color: #a7a08f; }

/* Response */

.response { display: flex; flex-direction: column; min-height: 0; }
.response-meta { display: flex; align-items: center; gap: 16px; padding: 12px 16px; border-bottom: 1.5px solid var(--line); font-family: var(--font-mono); font-size: 12px; }
.meta-item { display: flex; gap: 6px; }
.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 3px 10px;
  border: 1.5px solid var(--black);
  font-family: var(--font-mono);
  font-weight: 800;
  font-size: 12px;
}
.status-small { padding: 0 6px; font-size: 11px; }
.status-pill.status-ok { background: var(--green-soft); color: var(--green); border-color: var(--green); }
.status-pill.status-warn { background: var(--orange-soft); color: var(--orange); border-color: var(--orange); }
.status-pill.status-error { background: var(--red-soft); color: var(--red); border-color: var(--red); }
.status-pill.status-redirect { background: var(--blue-soft); color: var(--blue); border-color: var(--blue); }
.history-select { border: 1.5px solid var(--black); background: var(--white); padding: 3px 6px; font-family: var(--font-mono); font-size: 11px; }
.toggle { display: flex; align-items: center; gap: 6px; font-family: var(--font-mono); font-size: 11px; color: var(--gray); }
.toggle input { accent-color: var(--black); }
.response-body { padding-top: 12px; }
.response-empty {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 10px;
  padding: 48px 24px;
  color: var(--gray);
  text-align: center;
  min-height: 220px;
}
.big-icon {
  display: grid;
  place-items: center;
  width: 56px;
  height: 56px;
  border: var(--border);
  background: var(--yellow);
  box-shadow: var(--shadow);
  font-size: 26px;
  color: var(--black);
}
.error-banner { margin-bottom: 10px; padding: 8px 12px; border: 1.5px solid var(--red); background: var(--red-soft); color: var(--red); font-family: var(--font-mono); font-weight: 700; }
.request-line { font-family: var(--font-mono); word-break: break-all; }

.json-tree { font-family: var(--font-mono); font-size: 12.5px; line-height: 20px; }
.json-line { white-space: pre-wrap; word-break: break-word; padding-left: 14px; }
.json-toggle { cursor: pointer; padding-left: 0; }
.json-toggle:hover { background: var(--yellow-soft); }
.json-children { padding-left: 16px; border-left: 1px dashed var(--line); margin-left: 4px; }
.json-caret { display: inline-block; width: 14px; transition: transform 0.12s; color: var(--gray); }
.json-caret.open { transform: rotate(90deg); }
.json-summary { color: var(--gray); font-style: italic; padding: 0 4px; }

.headers-table { width: 100%; border-collapse: collapse; font-family: var(--font-mono); font-size: 12px; }
.headers-table th, .headers-table td { text-align: left; padding: 5px 8px; border-bottom: 1px solid var(--line); vertical-align: top; }
.headers-table th { color: var(--gray); font-weight: 600; width: 38%; white-space: nowrap; }
.headers-table td { word-break: break-all; }

.card { border: var(--border); background: var(--white); padding: 20px 24px; box-shadow: var(--shadow); }
.error-card h2, .error-card h3 { margin-top: 0; font-family: var(--font-mono); }
.fullscreen-message { height: 100%; display: grid; place-items: center; padding: 24px; }
.fullscreen-message .card { max-width: 480px; text-align: center; display: flex; flex-direction: column; align-items: center; }

.loader { width: 28px; height: 28px; border: 3px solid var(--black); border-right-color: var(--yellow); border-radius: 50%; animation: spin 0.7s linear infinite; }
.spinner { width: 14px; height: 14px; border: 2px solid var(--black); border-right-color: transparent; border-radius: 50%; animation: spin 0.6s linear infinite; display: inline-block; }
.spinner-tiny { width: 9px; height: 9px; border-width: 1.5px; }
@keyframes spin { to { transform: rotate(360deg); } }

/* Jobs */

.job-runs { border-top: var(--border); }
.job-runs-header { display: flex; align-items: center; gap: 8px; padding: 12px 16px; border-bottom: 1.5px solid var(--line); font-family: var(--font-mono); font-size: 11px; }
.job-runs-header h3 { margin: 0 8px 0 0; font-size: 13px; }
.state-badge {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  padding: 1px 8px;
  border: 1.5px solid currentColor;
  font-family: var(--font-mono);
  font-size: 10.5px;
  font-weight: 800;
  min-width: 82px;
  justify-content: center;
}
.state-created { color: var(--gray); background: var(--cream); }
.state-retry { color: var(--orange); background: var(--orange-soft); }
.state-active { color: var(--blue); background: var(--blue-soft); }
.state-completed { color: var(--green); background: var(--green-soft); }
.state-failed, .state-expired { color: var(--red); background: var(--red-soft); }
.state-cancelled { color: var(--gray); background: var(--white); }
.runs-list { display: flex; flex-direction: column; }
.run { border-bottom: 1px solid var(--line); }
.run-new { animation: flash 1.6s ease-out; }
@keyframes flash { from { background: var(--yellow); } to { background: transparent; } }
.run-row { display: flex; align-items: center; gap: 12px; width: 100%; padding: 8px 16px; border: none; background: none; text-align: left; font-family: var(--font-mono); font-size: 12px; }
.run-row:hover { background: var(--yellow-soft); }
.run-id { min-width: 70px; }
.run-time { flex: 1; }
.run-duration { min-width: 70px; text-align: right; }
.run-details { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; padding: 4px 16px 14px 32px; }

/* Traffic */

.traffic { display: flex; flex-direction: column; height: 100%; min-height: 0; }
.traffic-toolbar { display: flex; align-items: center; gap: 12px; padding: 12px 16px; border-bottom: var(--border); background: var(--white); }
.traffic-search { max-width: 260px; padding: 5px 10px; font-family: var(--font-mono); font-size: 12px; }
.traffic-split { flex: 1; display: flex; min-height: 0; }
.traffic-table { flex: 1; overflow-y: auto; min-width: 0; }
.traffic-row {
  display: grid;
  grid-template-columns: 90px 70px minmax(0, 1fr) 60px 180px 170px;
  align-items: center;
  gap: 10px;
  width: 100%;
  padding: 7px 16px;
  border: none;
  border-bottom: 1px solid var(--line);
  background: none;
  text-align: left;
  font-size: 12px;
  animation: slide-in 0.25s ease-out;
}
@keyframes slide-in { from { background: var(--yellow-soft); } to { background: transparent; } }
.traffic-row:hover { background: var(--yellow-soft); }
.traffic-row.active { background: var(--yellow); }
.traffic-head { position: sticky; top: 0; z-index: 1; background: var(--cream); font-family: var(--font-mono); font-size: 10px; text-transform: uppercase; letter-spacing: 0.1em; color: var(--gray); border-bottom: 1.5px solid var(--black); animation: none; cursor: default; }
.traffic-head:hover { background: var(--cream); }
.traffic-path { display: flex; gap: 8px; align-items: baseline; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; font-family: var(--font-mono); }
.traffic-user { overflow: hidden; white-space: nowrap; text-overflow: ellipsis; font-family: var(--font-mono); font-size: 11px; }
.duration-cell { display: flex; align-items: center; gap: 8px; font-size: 11px; }
.duration-track { flex: 1; height: 6px; }
.duration-bar { display: block; height: 100%; background: var(--yellow); border: 1px solid var(--black); }
.duration-label { min-width: 54px; text-align: right; }
.traffic-detail { width: 420px; flex-shrink: 0; border-left: var(--border); background: var(--paper); display: flex; flex-direction: column; min-height: 0; }
.traffic-detail-header { display: flex; align-items: center; gap: 10px; padding: 12px 14px; border-bottom: 1.5px solid var(--line); }
.traffic-detail-path { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.traffic-detail-meta { display: flex; flex-wrap: wrap; gap: 6px 16px; white-space: nowrap; padding: 10px 14px; font-family: var(--font-mono); font-size: 11px; border-bottom: 1.5px solid var(--line); }
.traffic-detail > .btn-block { margin: 12px 14px 0; width: auto; }
.traffic-detail-body { flex: 1; overflow-y: auto; padding: 4px 14px 16px; }
.traffic-detail-body details { margin-top: 10px; }
.traffic-detail-body summary { cursor: pointer; }
`;
