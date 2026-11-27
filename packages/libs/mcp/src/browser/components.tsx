import {
  clsx,
  FormMessageError,
  Message,
  SubmitButton,
  tokenObjToCSSVars,
  type CustomizationOptions,
} from "@wasp/lib-sdk-core/browser";
import * as React from "react";
import type { McpAppCall } from "../appEndpoints";
import { toggleScope } from "../scopes";
import type { ConnectionView, PendingActionView, ScopeView } from "../views";
import styles from "./McpComponents.module.css";

/**
 * Styling for the MCP components, the same as the auth forms take: `logo`, and
 * `appearance.colors` / `appearance.fontSizes` (e.g. `brand` colors the Allow
 * button). Every element also has a `wasp-mcp-*` class name.
 */
export type McpCustomizationOptions = Pick<
  CustomizationOptions,
  "logo" | "appearance"
>;

export type McpComponentsOptions = {
  /** Calls the app's `/mcp-app` endpoints in the user's session. */
  call: McpAppCall;
  /** The URL users add to an AI client, shown on the connections page. */
  mcpServerUrl: string;
  /** Whether any tool waits for approval, which is when "Ask me before…" means something. */
  hasDestructiveTools: boolean;
  /** Runs when the consent or confirm page opens, e.g. to forget a return-after-login. */
  onPageOpened?: () => void;
};

const errorMessage = (e: unknown) =>
  e instanceof Error ? e.message : "Something went wrong.";

type Loadable<T> =
  | { status: "loading" }
  | { status: "ready"; data: T }
  | { status: "error"; message: string };

/** Loads once on mount. `setData` replaces the data after a change. */
function useLoad<T>(load: () => Promise<T>) {
  const [state, setState] = React.useState<Loadable<T>>({ status: "loading" });
  React.useEffect(() => {
    load()
      .then((data) => setState({ status: "ready", data }))
      .catch((e) => setState({ status: "error", message: errorMessage(e) }));
    // Loads once: `load` only reads values fixed for the page's lifetime.
  }, []);
  const setData = (update: (data: T) => T) =>
    setState((s) =>
      s.status === "ready" ? { ...s, data: update(s.data) } : s,
    );
  return { state, setData };
}

/** Page frame: logo, the loading and load-error states, then the content. */
function Shell<T>({
  className,
  logo,
  appearance,
  state,
  loadingText,
  errorTitle,
  children,
}: McpCustomizationOptions & {
  className: string;
  state: Loadable<T>;
  loadingText: string;
  errorTitle: string;
  children: (data: T) => React.ReactNode;
}) {
  const appearanceStyle = React.useMemo(
    () => ({
      ...tokenObjToCSSVars("color", appearance?.colors ?? {}),
      ...tokenObjToCSSVars("font-size", appearance?.fontSizes ?? {}),
    }),
    [appearance],
  );
  return (
    <div className={clsx(className, styles.container)} style={appearanceStyle}>
      {logo && <img className={styles.logo} src={logo} alt="" />}
      {state.status === "loading" && (
        <Message role="status">{loadingText}</Message>
      )}
      {state.status === "error" && (
        <>
          {errorTitle && <h2 className={styles.title}>{errorTitle}</h2>}
          <FormMessageError role="alert">{state.message}</FormMessageError>
        </>
      )}
      {state.status === "ready" && children(state.data)}
    </div>
  );
}

function ClientPanel({
  className,
  name,
  host,
}: {
  className: string;
  name: string;
  host: string;
}) {
  return (
    <div className={clsx(className, styles.panel)}>
      <strong>{name}</strong>
      <span className={styles.muted}>{host}</span>
    </div>
  );
}

function DecisionButtons({
  classPrefix,
  approveLabel,
  disabled,
  approveDisabled,
  onDecide,
}: {
  classPrefix: string;
  approveLabel: string;
  disabled: boolean;
  approveDisabled?: boolean;
  onDecide: (decision: "approve" | "deny") => void;
}) {
  return (
    <div className={clsx(`${classPrefix}-actions`, styles.actions)}>
      <button
        type="button"
        className={clsx(`${classPrefix}-deny`, styles.secondaryButton)}
        disabled={disabled}
        onClick={() => onDecide("deny")}
      >
        Deny
      </button>
      <SubmitButton
        type="button"
        className={`${classPrefix}-approve`}
        disabled={disabled || approveDisabled}
        onClick={() => onDecide("approve")}
      >
        {approveLabel}
      </SubmitButton>
    </div>
  );
}

const toDefinitions = (scopes: ScopeView[]) =>
  Object.fromEntries(
    scopes.map((s) => [s.scope, { label: s.label, implies: s.implies }]),
  );

/**
 * One checkbox per scope. Ticking a scope ticks what it implies; unticking one
 * unticks the scopes that imply it.
 */
function ScopeChecklist({
  scopes,
  selected,
  onChange,
  disabled,
  legend,
}: {
  scopes: ScopeView[];
  selected: string[];
  onChange: (selected: string[]) => void;
  disabled?: boolean;
  legend: string;
}) {
  const definitions = React.useMemo(() => toDefinitions(scopes), [scopes]);
  return (
    <fieldset
      className={clsx("wasp-mcp-scopes", styles.checkboxList)}
      disabled={disabled}
    >
      <legend className={styles.legend}>{legend}</legend>
      {scopes.map((scope) => (
        <label
          key={scope.scope}
          className={clsx("wasp-mcp-scope", styles.checkboxOption)}
        >
          <input
            type="checkbox"
            className={styles.checkbox}
            checked={selected.includes(scope.scope)}
            onChange={(e) =>
              onChange(
                toggleScope(
                  definitions,
                  selected,
                  scope.scope,
                  e.target.checked,
                ),
              )
            }
          />
          <span>
            {scope.label}
            {scope.description && (
              <span className={styles.hint}>{scope.description}</span>
            )}
          </span>
        </label>
      ))}
    </fieldset>
  );
}

/** On means the client's destructive tool calls wait for the user's approval. */
function ConfirmationToggle({
  checked,
  onChange,
  disabled,
}: {
  checked: boolean;
  onChange: (checked: boolean) => void;
  disabled?: boolean;
}) {
  return (
    <label
      className={clsx("wasp-mcp-require-confirmation", styles.checkboxOption)}
    >
      <input
        type="checkbox"
        className={styles.checkbox}
        checked={checked}
        disabled={disabled}
        onChange={(e) => onChange(e.target.checked)}
      />
      <span>
        Ask me before it does anything that can't be undone
        <span className={styles.hint}>
          Like deleting something. You approve each one in this app.
        </span>
      </span>
    </label>
  );
}

const formatDate = (iso: string) => new Date(iso).toLocaleDateString();

/** The consent, connections and confirm components, bound to the app's endpoints. */
export function createMcpComponents({
  call,
  mcpServerUrl,
  hasDestructiveTools,
  onPageOpened,
}: McpComponentsOptions) {
  /**
   * The consent screen MCP clients send users to. Render it on the page that
   * `app.mcp.consentRoute` points to. It shows which app is asking (and its
   * verified domain), what it may do, and lets the user allow or deny it.
   */
  function McpConsentForm({
    appName,
    logo,
    appearance,
  }: { appName?: string } & McpCustomizationOptions) {
    // The consent page is the OAuth authorization endpoint: the request is its query.
    const query = React.useMemo(() => window.location.search, []);
    const [selectedScopes, setSelectedScopes] = React.useState<string[]>([]);
    const [requireConfirmation, setRequireConfirmation] = React.useState(true);
    const [submitting, setSubmitting] = React.useState(false);
    const [submitError, setSubmitError] = React.useState<string | null>(null);

    const { state } = useLoad(async () => {
      onPageOpened?.();
      const request = await call("authorize/describe", { query });
      // Problems the client should hear about go back to it. A bad client or
      // redirect URI throws instead, so it's shown here and never redirected.
      if (!request.ok) {
        window.location.assign(request.redirectTo);
        return new Promise<never>(() => {});
      }
      // What the client asked for, plus what this user already granted it.
      const existing = request.existingConnection?.scopes ?? [];
      setSelectedScopes(
        request.scopes
          .map((s) => s.scope)
          .filter(
            (s) => request.requestedScopes.includes(s) || existing.includes(s),
          ),
      );
      setRequireConfirmation(
        request.existingConnection?.requireConfirmation ?? true,
      );
      return request;
    });

    async function decide(decision: "approve" | "deny") {
      setSubmitting(true);
      setSubmitError(null);
      try {
        const { redirectTo } =
          decision === "approve"
            ? await call("authorize/approve", {
                query,
                scopes: selectedScopes,
                requireConfirmation,
              })
            : await call("authorize/deny", { query });
        window.location.assign(redirectTo);
      } catch (e) {
        setSubmitting(false);
        setSubmitError(errorMessage(e));
      }
    }

    return (
      <Shell
        className="wasp-mcp-consent"
        logo={logo}
        appearance={appearance}
        state={state}
        loadingText="Checking the request…"
        errorTitle="This app can't connect"
      >
        {(request) => (
          <>
            <h2 className={styles.title}>Connect {request.clientName}?</h2>
            <ClientPanel
              className="wasp-mcp-consent-client"
              name={request.clientName}
              host={request.clientHost}
            />
            <p className={styles.text}>
              It will be able to use {appName ?? "your account"} as you, through
              the tools this app makes available to AI agents.
            </p>
            {request.scopes.length > 0 && (
              <ScopeChecklist
                scopes={request.scopes}
                selected={selectedScopes}
                onChange={setSelectedScopes}
                disabled={submitting}
                legend="Allow it to:"
              />
            )}
            {hasDestructiveTools && (
              <ConfirmationToggle
                checked={requireConfirmation}
                onChange={setRequireConfirmation}
                disabled={submitting}
              />
            )}
            <p className={styles.muted}>
              Only allow apps you trust. You can change this or disconnect it at
              any time.
            </p>
            {submitError && (
              <FormMessageError role="alert">{submitError}</FormMessageError>
            )}
            <DecisionButtons
              classPrefix="wasp-mcp-consent"
              approveLabel="Allow"
              disabled={submitting}
              // With scopes, allowing nothing would connect a client that can't do anything.
              approveDisabled={
                request.scopes.length > 0 && selectedScopes.length === 0
              }
              onDecide={decide}
            />
          </>
        )}
      </Shell>
    );
  }

  /**
   * The user's connected AI apps, each with a Disconnect button and, if the
   * app declares scopes, what it's allowed. Plus the MCP URL to connect a new one.
   */
  function McpConnections({
    appearance,
  }: Pick<McpCustomizationOptions, "appearance">) {
    const { state, setData } = useLoad(() => call("connections/list", {}));
    // Clients with a change in flight; their controls are disabled meanwhile.
    const [busy, setBusy] = React.useState<ReadonlySet<string>>(new Set());
    const [changeError, setChangeError] = React.useState<string | null>(null);

    async function change(
      clientId: string,
      run: () => Promise<(connections: ConnectionView[]) => ConnectionView[]>,
    ) {
      setBusy((b) => new Set(b).add(clientId));
      setChangeError(null);
      try {
        const apply = await run();
        setData((d) => ({ ...d, connections: apply(d.connections) }));
      } catch (e) {
        setChangeError(errorMessage(e));
      } finally {
        setBusy((b) => {
          const next = new Set(b);
          next.delete(clientId);
          return next;
        });
      }
    }

    const update = (
      clientId: string,
      patch: { scopes?: string[]; requireConfirmation?: boolean },
    ) =>
      change(clientId, async () => {
        const updated = await call("connections/update", {
          clientId,
          ...patch,
        });
        return (cs) => cs.map((c) => (c.clientId === clientId ? updated : c));
      });

    const revoke = (clientId: string) =>
      change(clientId, async () => {
        await call("connections/revoke", { clientId });
        return (cs) => cs.filter((c) => c.clientId !== clientId);
      });

    return (
      <Shell
        className="wasp-mcp-connections"
        appearance={appearance}
        state={state}
        loadingText="Loading…"
        errorTitle=""
      >
        {({ connections, scopes }) => (
          <>
            <section
              className={clsx("wasp-mcp-connect-card", styles.container)}
            >
              <h2 className={styles.sectionTitle}>Connect an AI agent</h2>
              <p className={styles.text}>
                Add this URL as a custom connector (MCP server) in your AI
                client:
              </p>
              <code className={styles.code}>{mcpServerUrl}</code>
            </section>
            <section className={styles.container}>
              <h2 className={styles.sectionTitle}>Connected apps</h2>
              {changeError && (
                <FormMessageError role="alert">{changeError}</FormMessageError>
              )}
              {connections.length === 0 ? (
                <p className={styles.muted}>No apps are connected.</p>
              ) : (
                <ul className={clsx("wasp-mcp-connection-list", styles.list)}>
                  {connections.map((connection) => {
                    const isBusy = busy.has(connection.clientId);
                    return (
                      <li
                        key={connection.clientId}
                        className={clsx(
                          "wasp-mcp-connection",
                          styles.connection,
                        )}
                      >
                        <div className={styles.connectionBody}>
                          <div>
                            <strong>{connection.clientName}</strong>
                            <p className={styles.muted}>
                              {connection.clientHost} · connected{" "}
                              {formatDate(connection.createdAt)}
                              {connection.lastUsedAt
                                ? ` · last used ${formatDate(connection.lastUsedAt)}`
                                : ""}
                            </p>
                          </div>
                          {scopes.length > 0 && (
                            // Only what the client asked for: a user can narrow or restore, never exceed it.
                            <ScopeChecklist
                              scopes={scopes.filter((s) =>
                                connection.requestedScopes.includes(s.scope),
                              )}
                              selected={connection.scopes}
                              onChange={(next) =>
                                update(connection.clientId, { scopes: next })
                              }
                              disabled={isBusy}
                              legend={`What ${connection.clientName} can do`}
                            />
                          )}
                          {hasDestructiveTools && (
                            <ConfirmationToggle
                              checked={connection.requireConfirmation}
                              onChange={(requireConfirmation) =>
                                update(connection.clientId, {
                                  requireConfirmation,
                                })
                              }
                              disabled={isBusy}
                            />
                          )}
                        </div>
                        <button
                          type="button"
                          className={clsx(
                            "wasp-mcp-connection-revoke",
                            styles.smallButton,
                          )}
                          aria-label={`Disconnect ${connection.clientName}`}
                          disabled={isBusy}
                          onClick={() => revoke(connection.clientId)}
                        >
                          Disconnect
                        </button>
                      </li>
                    );
                  })}
                </ul>
              )}
            </section>
          </>
        )}
      </Shell>
    );
  }

  /**
   * The page where users approve or deny a destructive tool call. Render it
   * on the page that `app.mcp.confirmRoute` points to; it reads the action
   * from the URL. Approving runs the tool right away, once.
   */
  function McpConfirmAction({ logo, appearance }: McpCustomizationOptions) {
    const actionId = React.useMemo(
      () => new URLSearchParams(window.location.search).get("action") ?? "",
      [],
    );
    const { state, setData } = useLoad(() => {
      onPageOpened?.();
      return call("actions/get", { actionId });
    });
    const [submitting, setSubmitting] = React.useState(false);
    const [submitError, setSubmitError] = React.useState<string | null>(null);

    async function decide(decision: "approve" | "deny") {
      setSubmitting(true);
      setSubmitError(null);
      try {
        const action = await call(
          decision === "approve" ? "actions/approve" : "actions/deny",
          { actionId },
        );
        setData(() => action);
      } catch (e) {
        setSubmitError(errorMessage(e));
      } finally {
        setSubmitting(false);
      }
    }

    return (
      <Shell
        className="wasp-mcp-confirm"
        logo={logo}
        appearance={appearance}
        state={state}
        loadingText="Loading the request…"
        errorTitle="Can't show this request"
      >
        {(action) => (
          <ConfirmBody
            action={action}
            submitting={submitting}
            submitError={submitError}
            onDecide={decide}
          />
        )}
      </Shell>
    );
  }

  return { McpConsentForm, McpConnections, McpConfirmAction };
}

function ConfirmBody({
  action,
  submitting,
  submitError,
  onDecide,
}: {
  action: PendingActionView;
  submitting: boolean;
  submitError: string | null;
  onDecide: (decision: "approve" | "deny") => void;
}) {
  const summary = (
    <div className={clsx("wasp-mcp-confirm-summary", styles.panel)}>
      <strong>{action.summary.title}</strong>
      {action.summary.details && (
        <span className={styles.muted}>{action.summary.details}</span>
      )}
    </div>
  );

  if (action.status === "pending") {
    return (
      <>
        <h2 className={styles.title}>Approve this action?</h2>
        <p className={styles.text}>
          An AI app connected to your account wants to do something that can't
          be undone:
        </p>
        {summary}
        <p className={styles.muted}>Requested by:</p>
        <ClientPanel
          className="wasp-mcp-confirm-client"
          name={action.clientName}
          host={action.clientHost}
        />
        <p className={styles.muted}>
          Nothing happens unless you approve. This request expires at{" "}
          {new Date(action.expiresAt).toLocaleTimeString()}.
        </p>
        {submitError && (
          <FormMessageError role="alert">{submitError}</FormMessageError>
        )}
        <DecisionButtons
          classPrefix="wasp-mcp-confirm"
          approveLabel="Approve"
          disabled={submitting}
          onDecide={onDecide}
        />
      </>
    );
  }

  const outcome =
    action.status === "approved"
      ? action.outcome && !action.outcome.ok
        ? {
            title: "Approved, but it failed",
            text: action.outcome.error.message,
            error: true,
          }
        : {
            title: "Approved",
            text: "Done. The AI app will see the result.",
            error: false,
          }
      : action.status === "denied"
        ? {
            title: "Denied",
            text: "Nothing was done. The AI app will be told.",
            error: false,
          }
        : {
            title: "Expired",
            text: "This request expired, so nothing was done.",
            error: false,
          };
  return (
    <>
      <h2 className={styles.title}>{outcome.title}</h2>
      {summary}
      {outcome.error ? (
        <FormMessageError role="alert">{outcome.text}</FormMessageError>
      ) : (
        <p className={styles.text}>{outcome.text}</p>
      )}
    </>
  );
}
