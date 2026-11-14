/// <reference lib="dom" />
import { serverUrl, type Endpoint, type SentRequest } from "./api";

export interface KeyValue {
  enabled: boolean;
  key: string;
  value: string;
}

export type AuthMode = "session" | "none" | "custom";

export interface Draft {
  body: string;
  pathParams: Record<string, string>;
  query: KeyValue[];
  headers: KeyValue[];
  authMode: AuthMode;
  customToken: string;
  delaySeconds: string;
}

export function emptyDraft(endpoint: Endpoint): Draft {
  return {
    body:
      endpoint.kind === "api" && !hasBody(endpoint.method) ? "" : "{\n  \n}",
    pathParams: {},
    query: [{ enabled: true, key: "", value: "" }],
    headers: [{ enabled: true, key: "", value: "" }],
    authMode: "session",
    customToken: "",
    delaySeconds: "",
  };
}

const DRAFT_PREFIX = "wasp-devtools:playground:draft:";

export function loadDraft(key: string, endpoint: Endpoint): Draft {
  try {
    const raw = localStorage.getItem(DRAFT_PREFIX + key);
    if (raw) return { ...emptyDraft(endpoint), ...JSON.parse(raw) };
  } catch {
    // Start fresh.
  }
  return emptyDraft(endpoint);
}

export function saveDraft(key: string, draft: Draft) {
  localStorage.setItem(DRAFT_PREFIX + key, JSON.stringify(draft));
}

export function hasBody(method: string) {
  return !["GET", "HEAD", "ALL"].includes(method.toUpperCase());
}

export function getPathParams(path: string): string[] {
  return [...path.matchAll(/:([A-Za-z0-9_]+)/g)].map((m) => m[1]!);
}

export function resolvePath(endpoint: Endpoint, draft: Draft): string {
  let path = endpoint.path.replace(
    /:([A-Za-z0-9_]+)/g,
    (whole, name: string) => {
      const value = draft.pathParams[name];
      return value ? encodeURIComponent(value) : whole;
    },
  );
  if (endpoint.kind === "api") {
    const params = new URLSearchParams();
    for (const row of draft.query) {
      if (row.enabled && row.key) params.append(row.key, row.value);
    }
    const search = params.toString();
    if (search) path += `?${search}`;
  }
  return path;
}

export function buildRequest(
  endpoint: Endpoint,
  draft: Draft,
  sessionId: string | null,
): { ok: true; request: SentRequest } | { ok: false; error: string } {
  const headers: Record<string, string> = {};
  let body: string | null = null;

  const parsedBody = parseBody(draft.body);
  if (!parsedBody.ok) return { ok: false, error: parsedBody.error };

  if (endpoint.kind === "query" || endpoint.kind === "action") {
    headers["Content-Type"] = "application/json";
    // Wasp serializes operation arguments with superjson.
    body = JSON.stringify(
      parsedBody.value === undefined ? {} : { json: parsedBody.value },
    );
  } else if (endpoint.kind === "job") {
    headers["Content-Type"] = "application/json";
    body = JSON.stringify({
      args: parsedBody.value ?? {},
      ...(draft.delaySeconds && { delaySeconds: Number(draft.delaySeconds) }),
    });
  } else if (hasBody(endpoint.method) && draft.body.trim()) {
    headers["Content-Type"] = "application/json";
    body = draft.body;
  }

  if (endpoint.kind !== "job") {
    const token =
      draft.authMode === "session"
        ? sessionId
        : draft.authMode === "custom"
          ? draft.customToken
          : null;
    if (token) headers["Authorization"] = `Bearer ${token}`;
  }

  for (const row of draft.headers) {
    if (row.enabled && row.key) headers[row.key] = row.value;
  }

  return {
    ok: true,
    request: {
      // APIs declared for all methods get a GET.
      method: endpoint.method === "ALL" ? "GET" : endpoint.method,
      url: serverUrl + resolvePath(endpoint, draft),
      headers,
      body,
    },
  };
}

function parseBody(
  text: string,
): { ok: true; value: unknown } | { ok: false; error: string } {
  if (!text.trim() || /^\{\s*\}$/.test(text.trim())) {
    return { ok: true, value: text.trim() ? {} : undefined };
  }
  try {
    return { ok: true, value: JSON.parse(text) };
  } catch (error) {
    return {
      ok: false,
      error: `The body isn't valid JSON: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
}

export function validateJson(text: string): string | null {
  const result = parseBody(text);
  return result.ok ? null : result.error;
}

export function toCurl(request: SentRequest): string {
  const quote = (s: string) => `'${s.replace(/'/g, `'\\''`)}'`;
  const parts = [`curl -X ${request.method} ${quote(request.url)}`];
  for (const [key, value] of Object.entries(request.headers)) {
    parts.push(`-H ${quote(`${key}: ${value}`)}`);
  }
  if (request.body) parts.push(`--data ${quote(request.body)}`);
  return parts.join(" \\\n  ");
}

export function toFetch(request: SentRequest): string {
  const init: Record<string, unknown> = { method: request.method };
  if (Object.keys(request.headers).length) init["headers"] = request.headers;
  const initText = JSON.stringify(init, null, 2);
  const bodyLine = request.body
    ? initText.replace(
        /\n}$/,
        `,\n  "body": JSON.stringify(${request.body})\n}`,
      )
    : initText;
  return `const response = await fetch(${JSON.stringify(request.url)}, ${bodyLine});\nconst data = await response.json();`;
}

export function toWaspSnippet(endpoint: Endpoint, draft: Draft): string {
  const args = draft.body.trim() || "{}";
  switch (endpoint.kind) {
    case "query":
      return `import { ${endpoint.name}, useQuery } from "wasp/client/operations";

// In a React component:
const { data, isLoading, error } = useQuery(${endpoint.name}, ${args});

// Or directly:
const result = await ${endpoint.name}(${args});`;
    case "action":
      return `import { ${endpoint.name} } from "wasp/client/operations";

const result = await ${endpoint.name}(${args});`;
    case "job":
      return `import { ${endpoint.name} } from "wasp/server/jobs";

const submittedJob = await ${endpoint.name}${draft.delaySeconds ? `.delay(${draft.delaySeconds})` : ""}.submit(${args});
const details = await submittedJob.pgBoss.details();`;
    case "api": {
      const method = endpoint.method.toLowerCase();
      const path = resolvePath(endpoint, draft);
      const json =
        hasBody(endpoint.method) && draft.body.trim()
          ? `, {\n  json: ${args},\n}`
          : "";
      return `import { api } from "wasp/client/api";

const data = await api.${method === "all" ? "get" : method}(${JSON.stringify(path.replace(/^\//, ""))}${json}).json();`;
    }
  }
}

export function matchesEndpoint(
  endpoint: Endpoint,
  method: string,
  path: string,
): boolean {
  if (endpoint.kind === "job") return false;
  if (endpoint.kind !== "api") return path === endpoint.path;
  if (endpoint.method !== "ALL" && endpoint.method !== method) return false;
  const pattern = new RegExp(
    "^" +
      endpoint.path
        .replace(/[.*+?^${}()|[\]\\]/g, "\\$&")
        .replace(/:([A-Za-z0-9_]+)/g, "[^/]+") +
      "/?$",
  );
  return pattern.test(path);
}
