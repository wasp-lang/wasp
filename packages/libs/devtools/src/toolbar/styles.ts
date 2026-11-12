// The toolbar lives in a shadow root, so these styles don't leak into the app
// and the app's styles don't leak into the toolbar. They follow the look of
// Wasp's starter templates: monospace type, hard black borders, and yellow
// offset shadows.
export const toolbarStyles = /* css */ `
:host {
  all: initial;
}

.wasp-devtools {
  --yellow: #f5c842;
  --black: #111;
  --cream: #f7f5f0;
  --white: #fafafa;
  --border: 2px solid var(--black);
  --font-mono: ui-monospace, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace;

  position: fixed;
  inset: 0;
  z-index: 2147483000;
  pointer-events: none;
  color: var(--black);
  font-family: var(--font-mono);
  font-size: 13px;
  line-height: 1.4;
}

.wasp-devtools *,
.wasp-devtools *::before,
.wasp-devtools *::after {
  box-sizing: border-box;
}

.bar,
.panel {
  pointer-events: auto;
}

/* Toolbar */

.bar {
  position: absolute;
  bottom: 16px;
  left: 50%;
  transform: translateX(-50%);
  display: flex;
  align-items: stretch;
  border: var(--border);
  background: var(--white);
  box-shadow: 4px 4px 0 var(--yellow);
}

.brand {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 0 10px 0 6px;
  border-right: var(--border);
  background: var(--yellow);
  color: var(--black);
  font-weight: 700;
  text-decoration: none;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.logo {
  display: flex;
  width: 22px;
  height: 22px;
}

.logo svg {
  width: 100%;
  height: 100%;
}

.dock-button {
  display: flex;
  align-items: center;
  gap: 6px;
  margin: 0;
  padding: 6px 10px;
  border: 0;
  border-right: var(--border);
  background: transparent;
  color: inherit;
  font: inherit;
  font-weight: 700;
  text-transform: uppercase;
  cursor: pointer;
}

.dock-button:last-child {
  border-right: 0;
}

.dock-button:hover {
  background: var(--cream);
}

.dock-button[aria-pressed="true"] {
  background: var(--yellow);
}

.icon {
  display: flex;
  flex: none;
  width: 18px;
  height: 18px;
}

.icon img {
  width: 100%;
  height: 100%;
}

/* Panel */

.panel {
  position: absolute;
  right: 16px;
  bottom: 72px;
  left: 16px;
  display: flex;
  flex-direction: column;
  height: min(70vh, 720px);
  border: var(--border);
  background: var(--white);
  box-shadow: 6px 6px 0 var(--yellow);
}

.panel-header {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 6px 6px 12px;
  border-bottom: var(--border);
}

.panel-title {
  flex: 1;
  margin: 0;
  font-size: 13px;
  text-transform: uppercase;
}

.panel-action {
  padding: 4px 8px;
  border: var(--border);
  background: var(--white);
  color: var(--black);
  font: inherit;
  font-weight: 700;
  text-decoration: none;
  cursor: pointer;
}

.panel-action:hover {
  background: var(--yellow);
}

.panel-body {
  flex: 1;
  min-height: 0;
  background: var(--cream);
}

.panel-frame {
  display: block;
  width: 100%;
  height: 100%;
  border: 0;
  background: var(--white);
}

/* Launcher */

.launcher {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 12px;
  height: 100%;
  padding: 24px;
  text-align: center;
}

.launcher .icon {
  width: 40px;
  height: 40px;
}

.launcher-title {
  margin: 0;
  font-size: 18px;
  text-transform: uppercase;
}

.launcher-description {
  max-width: 40ch;
  margin: 0;
}

.launcher-button {
  padding: 10px 20px;
  border: var(--border);
  background: var(--yellow);
  color: var(--black);
  font: inherit;
  font-weight: 700;
  cursor: pointer;
  box-shadow: 3px 3px 0 var(--black);
}

.launcher-button:active:not(:disabled) {
  transform: translate(3px, 3px);
  box-shadow: none;
}

.launcher-button:disabled {
  cursor: progress;
  opacity: 0.7;
}

.launcher-digest {
  max-width: 100%;
  margin: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  opacity: 0.7;
}

.launcher-error {
  max-width: 60ch;
  margin: 0;
  padding: 8px 12px;
  border: var(--border);
  background: #ffe1e1;
}
`;
