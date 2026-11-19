import { useEffect, useState } from "react";

// An app declaration, as the app's server serves them at `/__wasp/spec`. Wasp
// writes them with the same `{ declType, declValue }` envelope as the TS spec
// (see `packages/spec`). The types below are the parts Wasp Studio reads.
export type Decl = { declType: string; declValue: unknown };

export type Ref = { name: string; declType: string };

export type AppDecl = {
  name: string;
  auth: { userEntity: Ref; methods: Record<string, unknown> } | null;
};

export type PageDecl = { name: string; authRequired: boolean | null };

export type RouteDecl = { name: string; path: string; to: Ref };

export type OperationDecl = {
  name: string;
  entities: Ref[] | null;
  auth: boolean | null;
};

export type ApiDecl = {
  name: string;
  httpRoute: [method: string, path: string];
  entities: Ref[] | null;
  auth: boolean | null;
};

export type JobDecl = {
  name: string;
  schedule: { cron: string } | null;
  entities: Ref[] | null;
};

export type EntityDecl = { name: string };

const SPEC_ROUTE = "/__wasp/spec";

// The default port of the server that `wasp start` runs.
const DEFAULT_SERVER_URL = "http://localhost:3001";

// Wasp recompiles the app when its config changes, so we check for changes.
const POLL_INTERVAL_MS = 2000;

/**
 * Reads the app's declarations from its server, and keeps them up to date.
 * The server URL comes from the `serverUrl` query parameter, which the Wasp
 * Studio panel in Vite DevTools sets.
 */
export function useSpec() {
  const [decls, setDecls] = useState<Decl[] | null>(null);
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    const serverUrl =
      new URLSearchParams(window.location.search).get("serverUrl") ??
      DEFAULT_SERVER_URL;
    const specUrl = new URL(SPEC_ROUTE, serverUrl);

    let lastSpecText: string | null = null;
    let isStopped = false;

    async function fetchSpec() {
      try {
        const response = await fetch(specUrl);
        if (!response.ok) throw new Error(`${response.status}`);
        const specText = await response.text();
        if (isStopped) return;
        setIsConnected(true);
        // Only update when the spec changed, so the graph keeps its layout.
        if (specText !== lastSpecText) {
          lastSpecText = specText;
          setDecls(JSON.parse(specText) as Decl[]);
        }
      } catch {
        if (!isStopped) setIsConnected(false);
      }
    }

    fetchSpec();
    const interval = setInterval(fetchSpec, POLL_INTERVAL_MS);
    return () => {
      isStopped = true;
      clearInterval(interval);
    };
  }, []);

  return { decls, isConnected };
}
