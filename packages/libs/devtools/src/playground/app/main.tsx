/// <reference lib="dom" />
import { render } from "preact";
import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
} from "preact/hooks";

import {
  clearRecordedRequests,
  endpointKey,
  fetchCurrentUser,
  fetchJobRuns,
  fetchRecordedRequests,
  fetchStudioData,
  getAppSessionId,
  parseJson,
  send,
  serverUrl,
  subscribeToRequests,
  toEndpoints,
  type CurrentUser,
  type Endpoint,
  type JobRun,
  type ReceivedResponse,
  type RecordedRequest,
  type SentRequest,
  type StudioData,
} from "./api";
import { highlightJson, JsonEditor, JsonTree } from "./json";
import {
  buildRequest,
  getPathParams,
  hasBody,
  loadDraft,
  matchesEndpoint,
  resolvePath,
  saveDraft,
  toCurl,
  toFetch,
  toWaspSnippet,
  validateJson,
  type Draft,
  type KeyValue,
} from "./request";
import { playgroundStyles } from "./styles";

type View = "playground" | "traffic";

interface Session {
  id: string | null;
  user: CurrentUser | null;
}

function App() {
  const [data, setData] = useState<StudioData | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [view, setView] = useState<View>("playground");
  const [selectedKey, setSelectedKey] = useState<string | null>(() =>
    localStorage.getItem("wasp-devtools:playground:selected"),
  );
  const [traffic, setTraffic] = useState<RecordedRequest[]>([]);
  const [streamConnected, setStreamConnected] = useState(false);
  const [session, setSession] = useState<Session>({ id: null, user: null });
  const [replay, setReplay] = useState<{
    key: string;
    body: string;
    nonce: number;
  } | null>(null);

  const load = useCallback(() => {
    setLoadError(null);
    fetchStudioData()
      .then(setData)
      .catch((error) => setLoadError(String(error?.message ?? error)));
  }, []);
  useEffect(load, [load]);

  useEffect(() => {
    fetchRecordedRequests()
      .then(setTraffic)
      .catch(() => {});
    return subscribeToRequests(
      (request) => setTraffic((current) => [...current.slice(-299), request]),
      setStreamConnected,
    );
  }, [data !== null]);

  // Follow the app's session, so requests run as whoever is signed in.
  useEffect(() => {
    let lastId: string | null | undefined;
    const check = () => {
      const id = getAppSessionId();
      if (id === lastId) return;
      lastId = id;
      if (!id) return setSession({ id: null, user: null });
      fetchCurrentUser(id)
        .then((user) => setSession({ id, user }))
        .catch(() => setSession({ id, user: null }));
    };
    check();
    const interval = setInterval(check, 2000);
    window.addEventListener("storage", check);
    return () => {
      clearInterval(interval);
      window.removeEventListener("storage", check);
    };
  }, []);

  const endpoints = useMemo(() => (data ? toEndpoints(data) : []), [data]);
  const selected =
    endpoints.find((e) => endpointKey(e) === selectedKey) ?? null;

  const select = (key: string) => {
    setSelectedKey(key);
    setView("playground");
    localStorage.setItem("wasp-devtools:playground:selected", key);
  };

  if (loadError) {
    return (
      <div class="fullscreen-message">
        <div class="card error-card">
          <div class="big-icon">⚡</div>
          <h2>Can't reach the Wasp server</h2>
          <p>
            The API Playground talks to your server at <code>{serverUrl}</code>.
            Make sure <code>wasp start</code> is running and the server finished
            starting.
          </p>
          <p class="muted">{loadError}</p>
          <button class="btn btn-primary" onClick={load}>
            Try again
          </button>
        </div>
      </div>
    );
  }
  if (!data) {
    return (
      <div class="fullscreen-message">
        <div class="loader" />
      </div>
    );
  }

  return (
    <div class="app">
      <Header
        data={data}
        view={view}
        setView={setView}
        trafficCount={traffic.length}
        streamConnected={streamConnected}
        session={session}
      />
      <div class="layout">
        <Sidebar
          endpoints={endpoints}
          selectedKey={view === "playground" ? selectedKey : null}
          onSelect={select}
          traffic={traffic}
        />
        <main class="main">
          {view === "traffic" ? (
            <TrafficView
              traffic={traffic}
              endpoints={endpoints}
              onClear={() => {
                void clearRecordedRequests();
                setTraffic([]);
              }}
              onReplay={(endpoint, body) => {
                select(endpointKey(endpoint));
                setReplay({
                  key: endpointKey(endpoint),
                  body,
                  nonce: Date.now(),
                });
              }}
            />
          ) : selected ? (
            <EndpointView
              key={endpointKey(selected)}
              endpoint={selected}
              session={session}
              traffic={traffic}
              replay={replay?.key === endpointKey(selected) ? replay : null}
            />
          ) : (
            <Welcome data={data} endpoints={endpoints} onSelect={select} />
          )}
        </main>
      </div>
    </div>
  );
}

function Header({
  data,
  view,
  setView,
  trafficCount,
  streamConnected,
  session,
}: {
  data: StudioData;
  view: View;
  setView: (view: View) => void;
  trafficCount: number;
  streamConnected: boolean;
  session: Session;
}) {
  return (
    <header class="header">
      <div class="header-brand">
        <span class="header-logo">⇄</span>
        <div>
          <div class="header-title">API Playground</div>
          <div class="header-subtitle">{data.app.name}</div>
        </div>
      </div>
      <nav class="header-tabs">
        <button
          class={`header-tab ${view === "playground" ? "active" : ""}`}
          onClick={() => setView("playground")}
        >
          Playground
        </button>
        <button
          class={`header-tab ${view === "traffic" ? "active" : ""}`}
          onClick={() => setView("traffic")}
        >
          <span class={`live-dot ${streamConnected ? "on" : ""}`} />
          Live traffic
          {trafficCount > 0 && <span class="count-badge">{trafficCount}</span>}
        </button>
      </nav>
      <div class="header-right">
        <span class="server-chip" title="The Wasp server the playground calls">
          <span class={`status-dot ${streamConnected ? "ok" : "bad"}`} />
          {serverUrl.replace(/^https?:\/\//, "")}
        </span>
        {data.app.auth && (
          <span
            class={`session-chip ${session.id ? "signed-in" : ""}`}
            title={
              session.id
                ? "Requests use the session of the user signed in to the app."
                : "Sign in to the app to send requests as a user."
            }
          >
            <span class="session-avatar">
              {session.user
                ? session.user.label.slice(0, 1).toUpperCase()
                : "?"}
            </span>
            {session.id ? (session.user?.label ?? "Signed in") : "Anonymous"}
          </span>
        )}
      </div>
    </header>
  );
}

const KIND_LABELS: Record<Endpoint["kind"], string> = {
  query: "Queries",
  action: "Actions",
  api: "APIs",
  job: "Jobs",
};

function Sidebar({
  endpoints,
  selectedKey,
  onSelect,
  traffic,
}: {
  endpoints: Endpoint[];
  selectedKey: string | null;
  onSelect: (key: string) => void;
  traffic: RecordedRequest[];
}) {
  const [search, setSearch] = useState("");
  const searchRef = useRef<HTMLInputElement>(null);
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (
        event.key === "/" &&
        document.activeElement?.tagName !== "TEXTAREA" &&
        document.activeElement?.tagName !== "INPUT"
      ) {
        event.preventDefault();
        searchRef.current?.focus();
      }
    };
    document.addEventListener("keydown", onKey);
    return () => document.removeEventListener("keydown", onKey);
  }, []);

  const stats = useMemo(() => {
    const result = new Map<
      string,
      { count: number; total: number; lastStatus: number }
    >();
    for (const endpoint of endpoints) {
      const matching = traffic.filter((r) =>
        matchesEndpoint(endpoint, r.method, r.path),
      );
      if (matching.length) {
        result.set(endpointKey(endpoint), {
          count: matching.length,
          total: matching.reduce((sum, r) => sum + r.durationMs, 0),
          lastStatus: matching[matching.length - 1]!.status,
        });
      }
    }
    return result;
  }, [endpoints, traffic]);

  const query = search.trim().toLowerCase();
  const filtered = endpoints.filter(
    (e) =>
      !query ||
      e.name.toLowerCase().includes(query) ||
      e.path.toLowerCase().includes(query) ||
      e.entities.some((entity) => entity.toLowerCase().includes(query)),
  );

  return (
    <aside class="sidebar">
      <div class="search">
        <span class="search-icon">⌕</span>
        <input
          ref={searchRef}
          placeholder="Search endpoints…"
          value={search}
          onInput={(event) => setSearch(event.currentTarget.value)}
        />
        <kbd>/</kbd>
      </div>
      <div class="sidebar-list">
        {(["query", "action", "api", "job"] as const).map((kind) => {
          const items = filtered.filter((e) => e.kind === kind);
          if (!items.length) return null;
          return (
            <section key={kind} class="sidebar-group">
              <h3 class="sidebar-heading">
                {KIND_LABELS[kind]} <span class="muted">{items.length}</span>
              </h3>
              {items.map((endpoint) => {
                const key = endpointKey(endpoint);
                const stat = stats.get(key);
                return (
                  <button
                    key={key}
                    class={`sidebar-item ${selectedKey === key ? "active" : ""}`}
                    onClick={() => onSelect(key)}
                  >
                    <MethodBadge endpoint={endpoint} />
                    <span class="sidebar-item-text">
                      <span class="sidebar-item-name">
                        {endpoint.name}
                        {endpoint.auth && (
                          <span class="lock" title="Requires auth">
                            🔒
                          </span>
                        )}
                      </span>
                      <span class="sidebar-item-meta">
                        {endpoint.kind === "api"
                          ? endpoint.path
                          : endpoint.kind === "job"
                            ? endpoint.schedule
                              ? `⏱ ${endpoint.schedule}`
                              : "on demand"
                            : endpoint.entities.join(", ") || "no entities"}
                      </span>
                    </span>
                    {stat && (
                      <span
                        class="sidebar-stat"
                        title={`${stat.count} calls, ${Math.round(stat.total / stat.count)}ms on average`}
                      >
                        <span
                          class={`status-dot ${statusClass(stat.lastStatus)}`}
                        />
                        {stat.count}
                      </span>
                    )}
                  </button>
                );
              })}
            </section>
          );
        })}
        {!filtered.length && (
          <p class="empty muted">Nothing matches "{search}".</p>
        )}
      </div>
    </aside>
  );
}

function MethodBadge({
  endpoint,
  small,
}: {
  endpoint: Endpoint;
  small?: boolean;
}) {
  const label =
    endpoint.kind === "api" ? endpoint.method : endpoint.kind.toUpperCase();
  const tone =
    endpoint.kind === "api"
      ? `method-${endpoint.method.toLowerCase()}`
      : `kind-${endpoint.kind}`;
  return (
    <span class={`badge ${tone} ${small ? "badge-small" : ""}`}>{label}</span>
  );
}

function Welcome({
  data,
  endpoints,
  onSelect,
}: {
  data: StudioData;
  endpoints: Endpoint[];
  onSelect: (key: string) => void;
}) {
  const count = (kind: Endpoint["kind"]) =>
    endpoints.filter((e) => e.kind === kind).length;
  return (
    <div class="welcome">
      <h1>
        Call any part of <mark>{data.app.name}</mark>.
      </h1>
      <p class="muted">
        Pick a query, action, API, or job on the left to build a request, send
        it to your server, and inspect what comes back. Watch the requests your
        app makes in <b>Live traffic</b>.
      </p>
      <div class="welcome-stats">
        {(["query", "action", "api", "job"] as const).map((kind) => (
          <div key={kind} class={`stat-card kind-border-${kind}`}>
            <div class="stat-number">{count(kind)}</div>
            <div class="stat-label">{KIND_LABELS[kind]}</div>
          </div>
        ))}
      </div>
      <div class="welcome-quick">
        {endpoints.slice(0, 6).map((endpoint) => (
          <button
            key={endpointKey(endpoint)}
            class="quick-item"
            onClick={() => onSelect(endpointKey(endpoint))}
          >
            <MethodBadge endpoint={endpoint} small />
            {endpoint.name}
          </button>
        ))}
      </div>
      <p class="muted hint">
        <kbd>/</kbd> search &nbsp; <kbd>⌘</kbd>+<kbd>↵</kbd> send
      </p>
    </div>
  );
}

type RequestTab = "body" | "params" | "headers" | "auth" | "code";

// Responses per endpoint, kept while the page is open.
const responseHistory = new Map<string, ReceivedResponse[]>();

function EndpointView({
  endpoint,
  session,
  traffic,
  replay,
}: {
  endpoint: Endpoint;
  session: Session;
  traffic: RecordedRequest[];
  replay: { body: string; nonce: number } | null;
}) {
  const key = endpointKey(endpoint);
  const [draft, setDraftState] = useState<Draft>(() =>
    loadDraft(key, endpoint),
  );
  const setDraft = (update: Partial<Draft>) =>
    setDraftState((current) => {
      const next = { ...current, ...update };
      saveDraft(key, next);
      return next;
    });

  useEffect(() => {
    if (replay) setDraft({ body: replay.body });
  }, [replay?.nonce]);

  const pathParams = getPathParams(endpoint.path);
  const showBody = endpoint.kind !== "api" || hasBody(endpoint.method);
  const [tab, setTab] = useState<RequestTab>(showBody ? "body" : "params");
  const [history, setHistory] = useState<ReceivedResponse[]>(
    () => responseHistory.get(key) ?? [],
  );
  const [shownIndex, setShownIndex] = useState(0);
  const [sending, setSending] = useState(false);
  const [buildError, setBuildError] = useState<string | null>(null);
  const [lastJobId, setLastJobId] = useState<string | null>(null);

  const built = buildRequest(endpoint, draft, session.id);
  const jsonError = showBody ? validateJson(draft.body) : null;

  const sendRequest = async () => {
    if (!built.ok) {
      setBuildError(built.error);
      return;
    }
    setBuildError(null);
    setSending(true);
    const response = await send(built.request);
    setSending(false);
    const next = [response, ...history].slice(0, 20);
    responseHistory.set(key, next);
    setHistory(next);
    setShownIndex(0);
    if (endpoint.kind === "job" && response.json.ok) {
      setLastJobId(
        String((response.json.value as { jobId?: string }).jobId ?? ""),
      );
    }
  };

  // A layout effect, so the shortcut works right after picking an endpoint.
  useLayoutEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
        event.preventDefault();
        void sendRequest();
      }
    };
    document.addEventListener("keydown", onKey);
    return () => document.removeEventListener("keydown", onKey);
  });

  const calls = traffic.filter((r) =>
    matchesEndpoint(endpoint, r.method, r.path),
  );
  const avg = calls.length
    ? calls.reduce((s, r) => s + r.durationMs, 0) / calls.length
    : null;

  const tabs: {
    id: RequestTab;
    label: string;
    hidden?: boolean;
    dot?: boolean;
  }[] = [
    {
      id: "body",
      label: endpoint.kind === "api" ? "Body" : "Arguments",
      hidden: !showBody,
      dot: Boolean(jsonError),
    },
    {
      id: "params",
      label: endpoint.kind === "job" ? "Options" : "Params",
      hidden: endpoint.kind !== "api" && endpoint.kind !== "job",
    },
    { id: "headers", label: "Headers", hidden: endpoint.kind === "job" },
    { id: "auth", label: "Auth", hidden: endpoint.kind === "job" },
    { id: "code", label: "Code" },
  ];

  const shown = history[shownIndex] ?? null;

  return (
    <div class="endpoint">
      <div class="endpoint-header">
        <div class="endpoint-title-row">
          <MethodBadge endpoint={endpoint} />
          <h1 class="endpoint-title">{endpoint.name}</h1>
          {endpoint.auth && <span class="chip chip-auth">🔒 auth</span>}
          {endpoint.kind === "job" && endpoint.schedule && (
            <span class="chip">⏱ {endpoint.schedule}</span>
          )}
          {endpoint.entities.map((entity) => (
            <span key={entity} class="chip chip-entity">
              ◆ {entity}
            </span>
          ))}
          <span class="endpoint-stats">
            {calls.length > 0 && (
              <>
                <b>{calls.length}</b> calls · avg <b>{formatDuration(avg!)}</b>
              </>
            )}
          </span>
        </div>
        <div class="url-bar">
          <span
            class={`url-method ${endpoint.kind === "api" ? `method-${endpoint.method.toLowerCase()}` : "method-post"}`}
          >
            {endpoint.method}
          </span>
          <span class="url-text">
            <span class="muted">{serverUrl}</span>
            {renderPath(resolvePath(endpoint, draft))}
          </span>
          <button
            class="btn btn-primary btn-send"
            onClick={sendRequest}
            disabled={sending}
          >
            {sending ? (
              <span class="spinner" />
            ) : endpoint.kind === "job" ? (
              "Submit job"
            ) : (
              "Send"
            )}
            <span class="kbd-hint">⌘↵</span>
          </button>
        </div>
        {buildError && <div class="inline-error">{buildError}</div>}
      </div>

      <div class="split">
        <section class="pane request-pane">
          <div class="tabs">
            {tabs
              .filter((t) => !t.hidden)
              .map((t) => (
                <button
                  key={t.id}
                  class={`tab ${tab === t.id ? "active" : ""}`}
                  onClick={() => setTab(t.id)}
                >
                  {t.label}
                  {t.dot && <span class="tab-dot" />}
                </button>
              ))}
          </div>
          <div class="pane-body">
            {tab === "body" && (
              <div class="body-editor">
                <JsonEditor
                  value={draft.body}
                  onInput={(body) => setDraft({ body })}
                  placeholder={
                    endpoint.kind === "api"
                      ? "Request body (JSON)"
                      : "Arguments (JSON)"
                  }
                />
                <div class="editor-footer">
                  {jsonError ? (
                    <span class="text-error">
                      ✕ {jsonError.replace("The body isn't valid JSON: ", "")}
                    </span>
                  ) : (
                    <span class="text-ok">✓ Valid JSON</span>
                  )}
                  <span class="spacer" />
                  {endpoint.kind !== "api" && (
                    <span class="muted">
                      {endpoint.kind === "job"
                        ? "Passed to the job as its args"
                        : "Sent with superjson, like the Wasp client"}
                    </span>
                  )}
                  <button
                    class="btn btn-ghost btn-small"
                    disabled={Boolean(jsonError) || !draft.body.trim()}
                    onClick={() =>
                      setDraft({
                        body: JSON.stringify(JSON.parse(draft.body), null, 2),
                      })
                    }
                  >
                    Prettify
                  </button>
                </div>
              </div>
            )}
            {tab === "params" && endpoint.kind === "api" && (
              <div class="form-stack">
                {pathParams.length > 0 && (
                  <div>
                    <h4 class="form-heading">Path parameters</h4>
                    {pathParams.map((name) => (
                      <label key={name} class="kv-row">
                        <span class="kv-key-static">:{name}</span>
                        <input
                          class="input"
                          value={draft.pathParams[name] ?? ""}
                          placeholder={name}
                          onInput={(event) =>
                            setDraft({
                              pathParams: {
                                ...draft.pathParams,
                                [name]: event.currentTarget.value,
                              },
                            })
                          }
                        />
                      </label>
                    ))}
                  </div>
                )}
                <div>
                  <h4 class="form-heading">Query parameters</h4>
                  <KeyValueEditor
                    rows={draft.query}
                    onChange={(query) => setDraft({ query })}
                  />
                </div>
              </div>
            )}
            {tab === "params" && endpoint.kind === "job" && (
              <div class="form-stack">
                <label class="field">
                  <span class="form-heading">Delay (seconds)</span>
                  <input
                    class="input"
                    type="number"
                    min="0"
                    placeholder="Run right away"
                    value={draft.delaySeconds}
                    onInput={(event) =>
                      setDraft({ delaySeconds: event.currentTarget.value })
                    }
                  />
                  <span class="muted">
                    Like{" "}
                    <code>{endpoint.name}.delay(seconds).submit(args)</code>.
                  </span>
                </label>
              </div>
            )}
            {tab === "headers" && (
              <KeyValueEditor
                rows={draft.headers}
                onChange={(headers) => setDraft({ headers })}
              />
            )}
            {tab === "auth" && (
              <div class="form-stack">
                {(["session", "none", "custom"] as const).map((mode) => (
                  <label
                    key={mode}
                    class={`radio-card ${draft.authMode === mode ? "active" : ""}`}
                  >
                    <input
                      type="radio"
                      name="auth"
                      checked={draft.authMode === mode}
                      onChange={() => setDraft({ authMode: mode })}
                    />
                    <span>
                      <b>
                        {mode === "session"
                          ? "App session"
                          : mode === "none"
                            ? "Anonymous"
                            : "Custom session ID"}
                      </b>
                      <span class="muted">
                        {mode === "session"
                          ? session.id
                            ? ` As ${session.user?.label ?? "the signed-in user"}, like the app's own requests.`
                            : " No one is signed in to the app right now."
                          : mode === "none"
                            ? " Without an Authorization header."
                            : " Use another user's session."}
                      </span>
                    </span>
                  </label>
                ))}
                {draft.authMode === "custom" && (
                  <input
                    class="input mono"
                    placeholder="Session ID"
                    value={draft.customToken}
                    onInput={(event) =>
                      setDraft({ customToken: event.currentTarget.value })
                    }
                  />
                )}
              </div>
            )}
            {tab === "code" && (
              <CodeTab
                endpoint={endpoint}
                draft={draft}
                built={built.ok ? built.request : null}
              />
            )}
          </div>
        </section>

        <section class="pane response-pane">
          <ResponseView
            endpoint={endpoint}
            response={shown}
            history={history}
            shownIndex={shownIndex}
            setShownIndex={setShownIndex}
            sending={sending}
          />
          {endpoint.kind === "job" && (
            <JobRuns jobName={endpoint.name} highlightId={lastJobId} />
          )}
        </section>
      </div>
    </div>
  );
}

function renderPath(path: string) {
  return path.split(/(:[A-Za-z0-9_]+)/).map((part, i) =>
    part.startsWith(":") ? (
      <span key={i} class="url-param">
        {part}
      </span>
    ) : (
      part
    ),
  );
}

function KeyValueEditor({
  rows,
  onChange,
}: {
  rows: KeyValue[];
  onChange: (rows: KeyValue[]) => void;
}) {
  const update = (index: number, patch: Partial<KeyValue>) => {
    const next = rows.map((row, i) =>
      i === index ? { ...row, ...patch } : row,
    );
    const lastRow = next[next.length - 1];
    if (lastRow && (lastRow.key || lastRow.value))
      next.push({ enabled: true, key: "", value: "" });
    onChange(next);
  };
  return (
    <div class="kv-table">
      {rows.map((row, index) => (
        <div key={index} class={`kv-row ${row.enabled ? "" : "disabled"}`}>
          <input
            type="checkbox"
            checked={row.enabled}
            onChange={(e) =>
              update(index, { enabled: e.currentTarget.checked })
            }
          />
          <input
            class="input mono"
            placeholder="Key"
            value={row.key}
            onInput={(e) => update(index, { key: e.currentTarget.value })}
          />
          <input
            class="input mono"
            placeholder="Value"
            value={row.value}
            onInput={(e) => update(index, { value: e.currentTarget.value })}
          />
          <button
            class="btn btn-ghost btn-icon"
            title="Remove"
            disabled={rows.length === 1}
            onClick={() => onChange(rows.filter((_, i) => i !== index))}
          >
            ✕
          </button>
        </div>
      ))}
    </div>
  );
}

function CodeTab({
  endpoint,
  draft,
  built,
}: {
  endpoint: Endpoint;
  draft: Draft;
  built: SentRequest | null;
}) {
  const [lang, setLang] = useState<"wasp" | "curl" | "fetch">("wasp");
  const code =
    lang === "wasp"
      ? toWaspSnippet(endpoint, draft)
      : !built
        ? "// Fix the request body first."
        : lang === "curl"
          ? toCurl(built)
          : toFetch(built);
  const [copied, setCopied] = useState(false);
  return (
    <div class="code-tab">
      <div class="segmented">
        {(["wasp", "curl", "fetch"] as const).map((l) => (
          <button
            key={l}
            class={lang === l ? "active" : ""}
            onClick={() => setLang(l)}
          >
            {l === "wasp" ? "Wasp SDK" : l}
          </button>
        ))}
        <span class="spacer" />
        <button
          class="btn btn-ghost btn-small"
          onClick={() => {
            void navigator.clipboard.writeText(code);
            setCopied(true);
            setTimeout(() => setCopied(false), 1200);
          }}
        >
          {copied ? "Copied ✓" : "Copy"}
        </button>
      </div>
      <pre class="code-block">{code}</pre>
    </div>
  );
}

type ResponseTab = "pretty" | "raw" | "headers" | "request";

function ResponseView({
  endpoint,
  response,
  history,
  shownIndex,
  setShownIndex,
  sending,
}: {
  endpoint: Endpoint;
  response: ReceivedResponse | null;
  history: ReceivedResponse[];
  shownIndex: number;
  setShownIndex: (index: number) => void;
  sending: boolean;
}) {
  const [tab, setTab] = useState<ResponseTab>("pretty");
  const [unwrap, setUnwrap] = useState(true);

  if (!response) {
    return (
      <div class="response-empty">
        {sending ? (
          <div class="loader" />
        ) : (
          <>
            <div class="big-icon">↗</div>
            <p>
              Press <b>{endpoint.kind === "job" ? "Submit job" : "Send"}</b> to
              see the response here.
            </p>
          </>
        )}
      </div>
    );
  }

  const isOperation = endpoint.kind === "query" || endpoint.kind === "action";
  const json = response.json.ok ? response.json.value : undefined;
  const isSuperjson =
    isOperation &&
    json !== null &&
    typeof json === "object" &&
    "json" in (json as object);
  const prettyValue =
    isSuperjson && unwrap ? (json as { json: unknown }).json : json;

  return (
    <div class="response">
      <div class="response-meta">
        <span class={`status-pill ${statusClass(response.status)}`}>
          {response.status === 0 ? "ERR" : response.status}{" "}
          {response.statusText || httpStatusText(response.status)}
        </span>
        <span class="meta-item">
          <span class="muted">Time</span>{" "}
          <b>{formatDuration(response.durationMs)}</b>
        </span>
        <span class="meta-item">
          <span class="muted">Size</span> <b>{formatBytes(response.size)}</b>
        </span>
        <span class="spacer" />
        {history.length > 1 && (
          <select
            class="history-select"
            value={shownIndex}
            onChange={(event) =>
              setShownIndex(Number(event.currentTarget.value))
            }
          >
            {history.map((h, i) => (
              <option key={h.at} value={i}>
                {i === 0 ? "Latest" : `#${history.length - i}`} ·{" "}
                {h.status || "ERR"} · {formatDuration(h.durationMs)} ·{" "}
                {new Date(h.at).toLocaleTimeString()}
              </option>
            ))}
          </select>
        )}
      </div>
      <div class="tabs">
        {(["pretty", "raw", "headers", "request"] as const).map((t) => (
          <button
            key={t}
            class={`tab ${tab === t ? "active" : ""}`}
            onClick={() => setTab(t)}
          >
            {t === "pretty"
              ? "Pretty"
              : t === "raw"
                ? "Raw"
                : t === "headers"
                  ? `Headers`
                  : "Request"}
            {t === "headers" && (
              <span class="muted"> {Object.keys(response.headers).length}</span>
            )}
          </button>
        ))}
        <span class="spacer" />
        {tab === "pretty" && isSuperjson && (
          <label class="toggle">
            <input
              type="checkbox"
              checked={unwrap}
              onChange={(e) => setUnwrap(e.currentTarget.checked)}
            />
            Unwrap superjson
          </label>
        )}
      </div>
      <div class="pane-body response-body">
        {response.networkError ? (
          <div class="card error-card">
            <h3>The request didn't reach the server</h3>
            <p class="mono">{response.networkError}</p>
            <p class="muted">
              Is the server running? Check the server's output in the terminal.
            </p>
          </div>
        ) : tab === "pretty" ? (
          response.json.ok ? (
            <>
              {response.status >= 400 && <ErrorSummary value={json} />}
              <JsonTree value={prettyValue} />
            </>
          ) : response.bodyText ? (
            <pre class="code-block">{response.bodyText}</pre>
          ) : (
            <p class="muted">Empty response body.</p>
          )
        ) : tab === "raw" ? (
          <pre class="code-block">
            {response.json.ok
              ? highlightJson(response.bodyText)
              : response.bodyText || "(empty)"}
          </pre>
        ) : tab === "headers" ? (
          <HeadersTable headers={response.headers} />
        ) : (
          <div class="form-stack">
            <div class="request-line">
              <b>{response.request.method}</b> {response.request.url}
            </div>
            <HeadersTable headers={response.request.headers} />
            {response.request.body && (
              <pre class="code-block">
                {highlightJson(
                  (() => {
                    const parsed = parseJson(response.request.body);
                    return parsed.ok
                      ? JSON.stringify(parsed.value, null, 2)
                      : response.request.body;
                  })(),
                )}
              </pre>
            )}
          </div>
        )}
      </div>
    </div>
  );
}

function ErrorSummary({ value }: { value: unknown }) {
  const message =
    value && typeof value === "object" && "message" in value
      ? String((value as { message: unknown }).message)
      : null;
  if (!message) return null;
  return <div class="error-banner">✕ {message}</div>;
}

function HeadersTable({ headers }: { headers: Record<string, string> }) {
  const entries = Object.entries(headers);
  if (!entries.length) return <p class="muted">No headers.</p>;
  return (
    <table class="headers-table">
      <tbody>
        {entries.map(([key, value]) => (
          <tr key={key}>
            <th>{key}</th>
            <td>{value}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

function JobRuns({
  jobName,
  highlightId,
}: {
  jobName: string;
  highlightId: string | null;
}) {
  const [runs, setRuns] = useState<JobRun[] | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [expanded, setExpanded] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    let timer: ReturnType<typeof setTimeout>;
    const poll = async () => {
      try {
        const next = await fetchJobRuns(jobName);
        if (cancelled) return;
        setRuns(next);
        setError(null);
        const busy = next.some((r) =>
          ["created", "active", "retry"].includes(r.state),
        );
        timer = setTimeout(poll, busy ? 800 : 3000);
      } catch (e) {
        if (cancelled) return;
        setError(e instanceof Error ? e.message : String(e));
        timer = setTimeout(poll, 5000);
      }
    };
    void poll();
    return () => {
      cancelled = true;
      clearTimeout(timer);
    };
  }, [jobName, highlightId]);

  const counts = (runs ?? []).reduce<Record<string, number>>((acc, r) => {
    acc[r.state] = (acc[r.state] ?? 0) + 1;
    return acc;
  }, {});

  return (
    <div class="job-runs">
      <div class="job-runs-header">
        <h3>Runs</h3>
        {Object.entries(counts).map(([state, n]) => (
          <span key={state} class={`state-badge state-${state}`}>
            {n} {state}
          </span>
        ))}
        <span class="spacer" />
        <span class="muted">from pg-boss</span>
      </div>
      {error && <div class="inline-error">{error}</div>}
      {runs && runs.length === 0 && (
        <p class="muted pad">No runs yet. Submit the job to see it here.</p>
      )}
      {runs && runs.length > 0 && (
        <div class="runs-list">
          {runs.map((run) => {
            const duration =
              run.startedon && run.completedon
                ? new Date(run.completedon).getTime() -
                  new Date(run.startedon).getTime()
                : run.startedon
                  ? Date.now() - new Date(run.startedon).getTime()
                  : null;
            return (
              <div
                key={run.id}
                class={`run ${run.id === highlightId ? "run-new" : ""}`}
              >
                <button
                  class="run-row"
                  onClick={() =>
                    setExpanded(expanded === run.id ? null : run.id)
                  }
                >
                  <span class={`state-badge state-${run.state}`}>
                    {run.state === "active" && (
                      <span class="spinner spinner-tiny" />
                    )}
                    {run.state}
                  </span>
                  <span class="mono muted run-id">{run.id.slice(0, 8)}</span>
                  <span class="run-time">{timeAgo(run.createdon)}</span>
                  <span class="run-duration">
                    {duration !== null ? formatDuration(duration) : "—"}
                  </span>
                  {run.retrycount > 0 && (
                    <span class="chip">↻ {run.retrycount}</span>
                  )}
                  <span
                    class={`json-caret ${expanded === run.id ? "open" : ""}`}
                  >
                    ▸
                  </span>
                </button>
                {expanded === run.id && (
                  <div class="run-details">
                    <div>
                      <h4 class="form-heading">Args</h4>
                      <JsonTree value={run.data} />
                    </div>
                    <div>
                      <h4 class="form-heading">Output</h4>
                      {run.output === null ? (
                        <p class="muted">No output yet.</p>
                      ) : (
                        <JsonTree value={run.output} />
                      )}
                    </div>
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}

type TrafficFilter = "all" | "operations" | "apis" | "auth" | "errors";

function TrafficView({
  traffic,
  endpoints,
  onClear,
  onReplay,
}: {
  traffic: RecordedRequest[];
  endpoints: Endpoint[];
  onClear: () => void;
  onReplay: (endpoint: Endpoint, body: string) => void;
}) {
  const [filter, setFilter] = useState<TrafficFilter>("all");
  const [search, setSearch] = useState("");
  const [selectedId, setSelectedId] = useState<number | null>(null);
  const [paused, setPaused] = useState<RecordedRequest[] | null>(null);

  const source = paused ?? traffic;
  const rows = source
    .filter((r) => {
      if (filter === "operations") return r.path.startsWith("/operations/");
      if (filter === "auth") return r.path.startsWith("/auth/");
      if (filter === "apis")
        return (
          !r.path.startsWith("/operations/") && !r.path.startsWith("/auth/")
        );
      if (filter === "errors") return r.status >= 400;
      return true;
    })
    .filter(
      (r) => !search || r.path.toLowerCase().includes(search.toLowerCase()),
    )
    .slice()
    .reverse();
  const selected = source.find((r) => r.id === selectedId) ?? null;
  const maxDuration = Math.max(1, ...rows.map((r) => r.durationMs));
  const findEndpoint = (r: RecordedRequest) =>
    endpoints.find((e) => matchesEndpoint(e, r.method, r.path));

  return (
    <div class="traffic">
      <div class="traffic-toolbar">
        <div class="segmented">
          {(["all", "operations", "apis", "auth", "errors"] as const).map(
            (f) => (
              <button
                key={f}
                class={filter === f ? "active" : ""}
                onClick={() => setFilter(f)}
              >
                {f === "apis" ? "APIs" : f[0]!.toUpperCase() + f.slice(1)}
              </button>
            ),
          )}
        </div>
        <input
          class="input traffic-search"
          placeholder="Filter by path…"
          value={search}
          onInput={(e) => setSearch(e.currentTarget.value)}
        />
        <span class="spacer" />
        <button
          class="btn btn-ghost"
          onClick={() => setPaused(paused ? null : traffic)}
        >
          {paused ? "▶ Resume" : "❚❚ Pause"}
        </button>
        <button class="btn btn-ghost" onClick={onClear}>
          Clear
        </button>
      </div>
      <div class="traffic-split">
        <div class="traffic-table">
          <div class="traffic-row traffic-head">
            <span>Time</span>
            <span>Method</span>
            <span>Endpoint</span>
            <span>Status</span>
            <span>User</span>
            <span>Duration</span>
          </div>
          {rows.length === 0 && (
            <div class="response-empty">
              <div class="big-icon">〰</div>
              <p>
                Use your app, and the requests it makes to the server show up
                here.
              </p>
            </div>
          )}
          {rows.map((r) => {
            const endpoint = findEndpoint(r);
            return (
              <button
                key={r.id}
                class={`traffic-row ${selectedId === r.id ? "active" : ""}`}
                onClick={() => setSelectedId(r.id)}
              >
                <span class="muted mono">
                  {new Date(r.startedAt).toLocaleTimeString()}
                </span>
                <span>
                  {endpoint ? (
                    <MethodBadge endpoint={endpoint} small />
                  ) : (
                    <span
                      class={`badge badge-small method-${r.method.toLowerCase()}`}
                    >
                      {r.method}
                    </span>
                  )}
                </span>
                <span class="traffic-path">
                  {endpoint ? <b>{endpoint.name}</b> : null}
                  <span class="muted mono">{r.path}</span>
                </span>
                <span>
                  <span
                    class={`status-pill status-small ${statusClass(r.status)}`}
                  >
                    {r.status}
                  </span>
                </span>
                <span class="muted traffic-user">
                  {r.user ? (r.user.identity ?? `#${r.user.id}`) : "—"}
                </span>
                <span class="duration-cell">
                  <span class="duration-track">
                    <span
                      class="duration-bar"
                      style={{
                        width: `${Math.max(4, (r.durationMs / maxDuration) * 100)}%`,
                      }}
                    />
                  </span>
                  <span class="mono duration-label">
                    {formatDuration(r.durationMs)}
                  </span>
                </span>
              </button>
            );
          })}
        </div>
        {selected && (
          <TrafficDetail
            request={selected}
            endpoint={findEndpoint(selected)}
            onClose={() => setSelectedId(null)}
            onReplay={onReplay}
          />
        )}
      </div>
    </div>
  );
}

function TrafficDetail({
  request,
  endpoint,
  onClose,
  onReplay,
}: {
  request: RecordedRequest;
  endpoint: Endpoint | undefined;
  onClose: () => void;
  onReplay: (endpoint: Endpoint, body: string) => void;
}) {
  const response = request.responseBody
    ? parseJson(request.responseBody)
    : { ok: false as const };
  const isOperation =
    endpoint && (endpoint.kind === "query" || endpoint.kind === "action");
  const unwrap = (value: unknown) =>
    isOperation && value && typeof value === "object" && "json" in value
      ? (value as { json: unknown }).json
      : value;
  const requestArgs = unwrap(request.requestBody);
  return (
    <aside class="traffic-detail">
      <div class="traffic-detail-header">
        <span class={`status-pill ${statusClass(request.status)}`}>
          {request.status}
        </span>
        <b class="mono">{request.method}</b>
        <span class="mono traffic-detail-path">{request.path}</span>
        <span class="spacer" />
        <button class="btn btn-ghost btn-icon" onClick={onClose}>
          ✕
        </button>
      </div>
      <div class="traffic-detail-meta">
        <span>
          <span class="muted">Duration</span>{" "}
          <b>{formatDuration(request.durationMs)}</b>
        </span>
        <span>
          <span class="muted">Size</span>{" "}
          <b>{formatBytes(request.responseSize)}</b>
        </span>
        <span>
          <span class="muted">User</span>{" "}
          <b>
            {request.user
              ? (request.user.identity ?? request.user.id)
              : "anonymous"}
          </b>
        </span>
      </div>
      {endpoint && (
        <button
          class="btn btn-primary btn-block"
          onClick={() =>
            onReplay(
              endpoint,
              requestArgs === undefined || requestArgs === null
                ? "{\n  \n}"
                : JSON.stringify(requestArgs, null, 2),
            )
          }
        >
          ↻ Open in playground
        </button>
      )}
      <div class="traffic-detail-body">
        <h4 class="form-heading">
          {isOperation ? "Arguments" : "Request body"}
        </h4>
        {requestArgs === undefined ||
        requestArgs === null ||
        (typeof requestArgs === "object" &&
          !Object.keys(requestArgs as object).length) ? (
          <p class="muted">None</p>
        ) : (
          <JsonTree value={requestArgs} />
        )}
        {Object.keys(request.query).length > 0 && (
          <>
            <h4 class="form-heading">Query</h4>
            <JsonTree value={request.query} />
          </>
        )}
        <h4 class="form-heading">Response</h4>
        {response.ok ? (
          <JsonTree value={unwrap(response.value)} />
        ) : (
          <pre class="code-block">{request.responseBody ?? "(empty)"}</pre>
        )}
        <details>
          <summary class="form-heading">Request headers</summary>
          <HeadersTable headers={request.requestHeaders} />
        </details>
        <details>
          <summary class="form-heading">Response headers</summary>
          <HeadersTable headers={request.responseHeaders} />
        </details>
      </div>
    </aside>
  );
}

function statusClass(status: number) {
  if (status === 0) return "status-error";
  if (status < 300) return "status-ok";
  if (status < 400) return "status-redirect";
  if (status < 500) return "status-warn";
  return "status-error";
}

function httpStatusText(status: number) {
  const texts: Record<number, string> = {
    200: "OK",
    201: "Created",
    204: "No Content",
    400: "Bad Request",
    401: "Unauthorized",
    403: "Forbidden",
    404: "Not Found",
    422: "Unprocessable",
    500: "Server Error",
  };
  return texts[status] ?? "";
}

function formatDuration(ms: number) {
  if (ms < 1) return `${ms.toFixed(2)} ms`;
  if (ms < 1000) return `${Math.round(ms)} ms`;
  return `${(ms / 1000).toFixed(2)} s`;
}

function formatBytes(bytes: number) {
  if (bytes < 1024) return `${bytes} B`;
  return `${(bytes / 1024).toFixed(1)} KB`;
}

function timeAgo(date: string) {
  const seconds = Math.round((Date.now() - new Date(date).getTime()) / 1000);
  if (seconds < 5) return "just now";
  if (seconds < 60) return `${seconds}s ago`;
  if (seconds < 3600) return `${Math.round(seconds / 60)}m ago`;
  if (seconds < 86400) return `${Math.round(seconds / 3600)}h ago`;
  return new Date(date).toLocaleDateString();
}

const style = document.createElement("style");
style.textContent = playgroundStyles;
document.head.append(style);
render(<App />, document.getElementById("app")!);
