import { useSyncExternalStore, type CSSProperties } from "react";
import { isRouteErrorResponse } from "react-router";
import type { ErrorPayload } from "vite";

type ViteError = ErrorPayload["err"];
type ViteErrorListener = (error: ViteError | null) => void;

class ViteErrorManager {
  #listeners = new Set<ViteErrorListener>();
  latestError: ViteError | null = null;

  constructor() {
    // This is disabled on production builds
    if (import.meta.hot) {
      import.meta.hot.on("vite:error", ({ err }) => {
        this.#onReceiveError(err);
      });
      import.meta.hot.on("vite:beforeUpdate", () => {
        this.#onReceiveError(null);
      });
    }
  }

  #onReceiveError(error: ViteError | null) {
    this.latestError = error;
    for (const listener of this.#listeners) {
      listener(error);
    }
  }

  // Using an arrow function here ensures that the function always retains the
  // correct `this`.
  subscribe = (listener: ViteErrorListener) => {
    this.#listeners.add(listener);
    return () => this.#listeners.delete(listener);
  };
}

const errorManager = new ViteErrorManager();

errorManager.subscribe((err) => {
  if (err) {
    console.error(formatViteError(err));
  }
});

export function ViteErrorDisplay({ outerError }: { outerError: unknown }) {
  const viteError = useSyncExternalStore(
    errorManager.subscribe,
    () => errorManager.latestError,
    () => null,
  );

  const displayedError =
    (outerError ? getErrorMessage(outerError) : null) ||
    (viteError ? formatViteError(viteError) : null);

  return displayedError ? (
    <pre style={detailsStyles}>{displayedError}</pre>
  ) : null;
}

const detailsStyles: CSSProperties = {
  maxWidth: "min(60rem, 90vw)",
  overflowX: "auto",
  whiteSpace: "pre-wrap",
};

function formatViteError(err: ViteError): string {
  const pluginPrefix = err.plugin ? `[plugin:${err.plugin}] ` : "";
  const location = err.loc
    ? `${err.loc.file ?? err.id}:${err.loc.line}:${err.loc.column}`
    : err.id;

  return [
    `[vite] ${pluginPrefix}${err.message}`,
    location && `  at ${location}`,
    err.frame,
  ]
    .filter(Boolean)
    .join("\n");
}

function getErrorMessage(error: unknown): string {
  if (isRouteErrorResponse(error)) {
    return `${error.status} ${error.statusText}`;
  }
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
}
