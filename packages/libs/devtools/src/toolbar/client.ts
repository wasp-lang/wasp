/// <reference lib="dom" />

// The Wasp DevTools toolbar. It runs in the app's page and replaces the
// floating dock of Vite DevTools, which `waspDevTools()` hides. It connects to
// the same Vite DevTools server, lists its panels, and shows them in a panel of
// its own. Panels it can't show (groups, custom renderers, ...) open in the
// full DevTools page instead.

import type {
  DevToolsDockEntry,
  DevToolsDockEntryIcon,
  DevToolsViewLauncher,
} from "@vitejs/devtools-kit";
import {
  getDevToolsRpcClient,
  type DevToolsRpcClient,
} from "@vitejs/devtools-kit/client";
import {
  DEFAULT_CATEGORIES_ORDER,
  DEVTOOLS_TERMINALS_DOCK_ID,
} from "@vitejs/devtools-kit/constants";
import { evaluateWhen } from "@vitejs/devtools-kit/utils/when";

import { toolbarStyles } from "./styles";

const DEVTOOLS_BASE = "/__devtools/";
const DOCKS_STATE_KEY = "devframe:docks";
// From `@devframes/plugin-terminals`, which `terminals()` adds.
const TERMINAL_SESSIONS_STATE_KEY = "devframes:plugin:terminals:sessions";

// The Wasp logo (`web/static/img/wasp-logo.svg` in the Wasp repo), without the
// background, so it can take the toolbar's colors.
const WASP_LOGO = `<svg viewBox="0 0 1520 1520" aria-hidden="true"><g fill="currentColor"><path d="m1007 300-156.98.002.499 142.51h85.857c.0239.002 18.763 1.566 30.461 7.9108 13.537 7.3425 14.626 10.763 21.704 20.417 3.0674 4.1835 4.9651 10.687 6.0494 16.123 1.0827 5.4284 1.3754 9.7642 1.3762 9.7762v144.33l.002.0252s.583 7.1093 1.796 15.918c1.213 8.809 3.0448 19.305 5.6079 26.205 4.3676 11.759 13.62 26.659 21.633 36.363 9.3866 11.367 22.228 20.124 34.969 27.568 11.294 6.5984 32.628 11.542 33.548 11.754v2.1847c-.9202.21233-22.254 5.1555-33.548 11.754-12.741 7.4439-25.583 16.201-34.97 27.568-8.0134 9.7041-17.266 24.604-21.633 36.363-2.5631 6.9007-4.3948 17.396-5.6078 26.205s-1.796 15.918-1.796 15.918l-.002.0271v144.33c-.00082.012-.29348 4.3478-1.3762 9.7762-1.0842 5.4359-2.982 11.94-6.0494 16.123-7.0789 9.6547-8.1671 13.075-21.704 20.418-11.698 6.3448-30.437 7.9107-30.461 7.9107h-85.857l-.499 142.51h156.99s7.5461-.1872 16.483-.629c8.9371-.4418 19.238-1.1296 24.889-2.1653 7.1809-1.3165 20.102-3.1616 33.53-9.0719 13.329-5.8668 16.299-7.3647 22.443-10.939 11.758-6.8408 24.169-19.146 34.441-32.311s18.389-27.167 21.463-37.524c6.1235-20.632 6.382-47.126 6.382-47.126v-195.36c0-.0207.145-2.5801.505-5.7396.3614-3.1722.956-6.9416 1.7742-9.1512.9504-2.5664 2.8911-7.2999 5.3504-11.905 2.4592-4.6049 5.481-9.0841 8.3701-11.115 5.643-3.9674 12.809-9.5822 20.29-10.61 5.9133-.81252 13.182-1.4397 13.202-1.4417h53.892v-149.83h-53.892c-.019-.002-7.2884-.62915-13.202-1.4417-7.4814-1.028-14.648-6.6429-20.29-10.61-2.8891-2.0312-5.9109-6.5104-8.3701-11.115-2.4593-4.6049-4.4-9.3384-5.3504-11.905-.8182-2.2095-1.4128-5.9789-1.7742-9.1512-.36-3.1595-.5038-5.7188-.505-5.7396v-195.36s-.2585-26.494-6.382-47.126c-3.0739-10.357-11.191-24.359-21.463-37.524s-22.683-25.47-34.441-32.311c-6.1438-3.5745-9.1141-5.0723-22.443-10.939-13.427-5.9102-26.349-7.7554-33.53-9.0718-5.6503-1.0358-15.952-1.7236-24.889-2.1654-8.9366-.4418-16.483-.62892-16.483-.62892z"/><path d="m250 830h500v160h-500z"/><path d="m250 530h500v160h-500z"/></g></svg>`;

type DockEntry = DevToolsDockEntry;

interface PanelActions {
  close: () => void;
  /** Shows another dock, passing `params` on to it (like a terminal session). */
  openDock: (dockId: string, params?: Record<string, unknown>) => void;
}

interface ToolbarState {
  docks: readonly DockEntry[];
  /** Ids of the processes the Terminals panel shows output for. */
  terminalSessionIds: ReadonlySet<string>;
  selectedDockId: string | null;
  // The rendered panel, kept across renders so its iframe doesn't reload.
  panel: { key: string; node: HTMLElement } | null;
}

void mountToolbar();

async function mountToolbar(): Promise<void> {
  const host = document.createElement("wasp-devtools");
  const root = host.attachShadow({ mode: "open" });
  const style = document.createElement("style");
  style.textContent = toolbarStyles;
  const container = element("div", "wasp-devtools");
  // Separate slots, so re-rendering the bar never moves the panel: moving an
  // iframe in the DOM reloads it.
  const panelSlot = element("div", "panel-slot");
  const barSlot = element("div", "bar-slot");
  container.append(panelSlot, barSlot);
  root.append(style, container);
  document.body.append(host);

  const rpc = await getDevToolsRpcClient({ baseURL: DEVTOOLS_BASE });
  const docksState = await rpc.sharedState.get<{ value?: DockEntry[] }>(
    DOCKS_STATE_KEY,
  );

  const state: ToolbarState = {
    docks: readDocks(docksState.value()),
    terminalSessionIds: new Set(),
    selectedDockId: null,
    panel: null,
  };
  const render = () =>
    renderToolbar({ panelSlot, barSlot }, state, rpc, render);

  docksState.on("updated", (fullState) => {
    state.docks = readDocks(fullState);
    render();
  });

  const sessionsState = await rpc.sharedState.get<object>(
    TERMINAL_SESSIONS_STATE_KEY,
  );
  const updateSessions = (sessions: unknown) => {
    state.terminalSessionIds = new Set(
      readList(sessions).map((session) => (session as { id: string }).id),
    );
    render();
  };
  sessionsState.on("updated", updateSessions);
  updateSessions(sessionsState.value());
  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape" && state.selectedDockId) {
      state.selectedDockId = null;
      render();
    }
  });

  render();
}

function readDocks(stateValue: unknown): readonly DockEntry[] {
  return readList(stateValue) as DockEntry[];
}

// Shared states that hold a list keep it at the top level or under a single
// key, depending on the hub version.
function readList(stateValue: unknown): readonly unknown[] {
  if (Array.isArray(stateValue)) return stateValue;
  if (stateValue && typeof stateValue === "object") {
    const list = Object.values(stateValue).find(Array.isArray);
    if (list) return list;
  }
  return [];
}

function getToolbarDocks(docks: readonly DockEntry[]): DockEntry[] {
  const visibleDocks = docks.filter(isVisible);
  const groupIds = new Set(
    visibleDocks.filter((dock) => dock.type === "group").map((dock) => dock.id),
  );
  const nonEmptyGroupIds = new Set(
    visibleDocks.flatMap((dock) => (dock.groupId ? [dock.groupId] : [])),
  );
  return visibleDocks
    .filter(
      (dock) =>
        // Vite DevTools' own views (settings, ...) stay in the full DevTools.
        dock.category !== "~builtin" &&
        // Group members show up through their group.
        !(dock.groupId && groupIds.has(dock.groupId)) &&
        // Vite DevTools registers its Vite+ group even when it's empty.
        !(dock.type === "group" && !nonEmptyGroupIds.has(dock.id)),
    )
    .sort(
      (a, b) =>
        // By category, then single panels before groups, then by the entry's
        // own `defaultOrder`, higher first.
        getCategoryOrder(a) - getCategoryOrder(b) ||
        Number(a.type === "group") - Number(b.type === "group") ||
        (b.defaultOrder ?? 0) - (a.defaultOrder ?? 0),
    );
}

function getCategoryOrder(dock: DockEntry): number {
  const categoryOrder: Record<string, number> = DEFAULT_CATEGORIES_ORDER;
  return categoryOrder[dock.category ?? "default"] ?? 0;
}

function isVisible(dock: DockEntry): boolean {
  if (!dock.when) return true;
  try {
    return evaluateWhen(dock.when, { clientType: "embedded" });
  } catch {
    return true;
  }
}

function renderToolbar(
  slots: { panelSlot: HTMLElement; barSlot: HTMLElement },
  state: ToolbarState,
  rpc: DevToolsRpcClient,
  rerender: () => void,
): void {
  const docks = getToolbarDocks(state.docks);
  const selectedDock = docks.find((dock) => dock.id === state.selectedDockId);

  if (selectedDock) {
    // Only re-create the panel when its dock changes.
    const hasOutput = state.terminalSessionIds.has(selectedDock.id);
    const key = JSON.stringify([selectedDock, hasOutput]);
    if (state.panel?.key !== key) {
      state.panel = {
        key,
        node: renderPanel(selectedDock, rpc, hasOutput, {
          close: () => {
            state.selectedDockId = null;
            rerender();
          },
          openDock: (dockId, params) => {
            // Docks that take params (like Terminals) read them from the
            // activation when they mount.
            void activateDock(rpc, dockId, params).then(() => {
              state.selectedDockId = dockId;
              rerender();
            });
          },
        }),
      };
      slots.panelSlot.replaceChildren(state.panel.node);
    }
  } else if (state.panel) {
    state.panel = null;
    slots.panelSlot.replaceChildren();
  }

  const bar = element("div", "bar");
  const brand = element("a", "brand");
  brand.setAttribute("href", DEVTOOLS_BASE);
  brand.setAttribute("target", "_blank");
  brand.title = "Open Wasp DevTools in a new tab";
  brand.innerHTML = `<span class="logo">${WASP_LOGO}</span><span class="brand-name">DevTools</span>`;
  bar.append(brand);

  for (const dock of docks) {
    const button = element("button", "dock-button");
    button.title = dock.title;
    button.setAttribute("aria-pressed", String(dock.id === selectedDock?.id));
    button.append(renderIcon(dock.icon), element("span", "label", dock.title));
    button.addEventListener("click", () => {
      state.selectedDockId = dock.id === state.selectedDockId ? null : dock.id;
      rerender();
    });
    bar.append(button);
  }

  slots.barSlot.replaceChildren(bar);
}

function renderPanel(
  dock: DockEntry,
  rpc: DevToolsRpcClient,
  hasOutput: boolean,
  actions: PanelActions,
): HTMLElement {
  const panel = element("section", "panel");

  const header = element("header", "panel-header");
  header.append(
    renderIcon(dock.icon),
    element("h2", "panel-title", dock.title),
  );
  const openFull = element("a", "panel-action", "Open in DevTools ↗");
  openFull.setAttribute("href", DEVTOOLS_BASE);
  openFull.setAttribute("target", "_blank");
  openFull.addEventListener("click", () => void activateDock(rpc, dock.id));
  const close = element("button", "panel-action", "Close ✕");
  close.addEventListener("click", actions.close);
  if (hasOutput) {
    // Processes that panels start (like `wasp studio`) use the dock's id for
    // their terminal session.
    const viewOutput = element("button", "panel-action", "View output");
    viewOutput.addEventListener("click", () =>
      actions.openDock(DEVTOOLS_TERMINALS_DOCK_ID, { sessionId: dock.id }),
    );
    header.append(viewOutput);
  }
  header.append(openFull, close);

  const body = element("div", "panel-body");
  body.append(renderDockContent(dock, rpc, actions));

  panel.append(header, body);
  return panel;
}

function renderDockContent(
  dock: DockEntry,
  rpc: DevToolsRpcClient,
  actions: PanelActions,
): Node {
  switch (dock.type) {
    case "iframe":
      return renderIframe(dock.url);
    case "launcher":
      return renderLauncher(dock as DevToolsViewLauncher, rpc, actions);
    default:
      // Anything else, the full DevTools page renders.
      void activateDock(rpc, dock.id);
      return renderIframe(DEVTOOLS_BASE);
  }
}

function renderIframe(url: string): HTMLIFrameElement {
  const iframe = element("iframe", "panel-frame");
  iframe.src = url;
  return iframe;
}

function renderLauncher(
  dock: DevToolsViewLauncher,
  rpc: DevToolsRpcClient,
  actions: PanelActions,
): HTMLElement {
  const { launcher } = dock;
  const card = element("div", "launcher");
  card.append(
    renderIcon(launcher.icon ?? dock.icon),
    element("h3", "launcher-title", launcher.title),
  );
  if (launcher.description) {
    card.append(element("p", "launcher-description", launcher.description));
  }

  const isLoading = launcher.status === "loading";
  const button = element(
    "button",
    "launcher-button",
    isLoading
      ? (launcher.buttonLoading ?? "Starting…")
      : (launcher.buttonStart ?? "Start"),
  );
  button.disabled = isLoading || !launcher.command;
  button.addEventListener("click", () => {
    if (launcher.command) {
      // The dock entry updates as the launch progresses, which re-renders.
      void rpc.call("hub:commands:execute", launcher.command).catch(() => {});
    }
  });
  card.append(button);

  const sessionId = launcher.terminalSessionId;
  if (sessionId) {
    const viewOutput = element("button", "panel-action", "View output");
    viewOutput.addEventListener("click", () =>
      actions.openDock(DEVTOOLS_TERMINALS_DOCK_ID, { sessionId }),
    );
    card.append(viewOutput);
  }

  if (launcher.digest) {
    card.append(element("pre", "launcher-digest", launcher.digest));
  }
  if (launcher.status === "error" && launcher.error) {
    card.append(element("p", "launcher-error", launcher.error));
  }
  return card;
}

async function activateDock(
  rpc: DevToolsRpcClient,
  dockId: string,
  params?: Record<string, unknown>,
): Promise<void> {
  try {
    await rpc.call("hub:docks:activate", { dockId, params });
  } catch {
    // The full DevTools page opens on its default view.
  }
}

function renderIcon(icon: DevToolsDockEntryIcon): HTMLElement {
  const wrapper = element("span", "icon");
  const src = typeof icon === "string" ? icon : icon.light;
  const img = element("img");
  img.alt = "";
  img.src = isIconifyName(src)
    ? `https://api.iconify.design/${src.replace(":", "/")}.svg`
    : src;
  wrapper.append(img);
  return wrapper;
}

function isIconifyName(icon: string): boolean {
  return /^[a-z0-9-]+:[a-z0-9-]+$/i.test(icon);
}

function element<Tag extends keyof HTMLElementTagNameMap>(
  tag: Tag,
  className?: string,
  text?: string,
): HTMLElementTagNameMap[Tag] {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}
