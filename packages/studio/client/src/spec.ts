import { useEffect, useState } from "react";

// The app's spec, as Wasp Studio reads it at `/__wasp/spec`: the same JSON
// that `wasp show spec --json` prints. The types below are the parts Wasp
// Studio reads.
export type Spec = { dbSystem: string; decls: Decl[] };

// Wasp writes declarations with the same `{ declType, declValue }` envelope as
// the TS spec (see `packages/spec`).
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

// Relative, so it resolves under the server URL's path, if it has one.
const SPEC_ROUTE = "__wasp/spec";

// The app's server serves a new spec when Wasp recompiles the app, so we check
// for changes.
const POLL_INTERVAL_MS = 2000;

/**
 * Reads the app's spec, and keeps it up to date. The Wasp Studio panel in Vite
 * DevTools sets the `serverUrl` query parameter, to read it from the server
 * that `wasp start` runs. Otherwise, it comes from the server that serves this
 * page, which `wasp studio` runs.
 */
export function useSpec() {
  const [spec, setSpec] = useState<Spec | null>(null);
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    const specUrl = getSpecUrl();
    if (!specUrl) return;

    let lastSpecText: string | null = null;
    let isStopped = false;

    const fetchSpec = async () => {
      try {
        const response = await fetch(specUrl);
        if (!response.ok) throw new Error(`${response.status}`);
        const specText = await response.text();
        if (isStopped) return;
        // Only update when the spec changed, so the graph keeps its layout.
        if (specText !== lastSpecText) {
          const parsed = JSON.parse(specText) as Partial<Spec> | null;
          if (!Array.isArray(parsed?.decls)) {
            throw new Error("The server didn't send the app's spec.");
          }
          lastSpecText = specText;
          setSpec(parsed as Spec);
        }
        setIsConnected(true);
      } catch {
        if (!isStopped) setIsConnected(false);
      }
    };

    fetchSpec();
    const interval = setInterval(fetchSpec, POLL_INTERVAL_MS);
    return () => {
      isStopped = true;
      clearInterval(interval);
    };
  }, []);

  return { spec, isConnected };
}

function getSpecUrl(): URL | null {
  const serverUrl =
    new URLSearchParams(window.location.search).get("serverUrl") ??
    new URL("./", window.location.href).href;
  try {
    // With a trailing slash, so the route goes under the server URL's path.
    return new URL(
      SPEC_ROUTE,
      serverUrl.endsWith("/") ? serverUrl : `${serverUrl}/`,
    );
  } catch {
    console.error(`Wasp Studio: \`${serverUrl}\` isn't a valid server URL.`);
    return null;
  }
}
