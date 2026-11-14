/// <reference lib="dom" />

declare global {
  interface Window {
    __WASP_PLAYGROUND__?: { serverUrl: string };
  }
}

export const serverUrl =
  window.__WASP_PLAYGROUND__?.serverUrl ?? "http://localhost:3001";

const DEVTOOLS_ROUTE = `${serverUrl}/__wasp/devtools`;

// The shape of `GET /__wasp/studio-data` (see `Wasp.Project.Studio`).
export interface StudioData {
  app: {
    name: string;
    auth: { userEntity: { name: string }; methods: string[] } | null;
    db: { system: string };
  };
  operations: {
    type: "query" | "action";
    name: string;
    entities: { name: string }[];
    auth: boolean | null;
  }[];
  apis: {
    name: string;
    httpRoute: { method: string; path: string };
    auth: boolean | null;
    entities: { name: string }[];
  }[];
  jobs: {
    name: string;
    schedule: string | null;
    entities: { name: string }[];
  }[];
  entities: { name: string }[];
}

export type Endpoint =
  | {
      kind: "query" | "action";
      name: string;
      entities: string[];
      auth: boolean;
      method: "POST";
      path: string;
    }
  | {
      kind: "api";
      name: string;
      entities: string[];
      auth: boolean;
      method: string;
      path: string;
    }
  | {
      kind: "job";
      name: string;
      entities: string[];
      auth: false;
      schedule: string | null;
      method: "POST";
      path: string;
    };

export function endpointKey(endpoint: Pick<Endpoint, "kind" | "name">) {
  return `${endpoint.kind}:${endpoint.name}`;
}

export function toEndpoints(data: StudioData): Endpoint[] {
  const authEnabled = Boolean(data.app.auth);
  return [
    ...data.operations.map(
      (op): Endpoint => ({
        kind: op.type,
        name: op.name,
        entities: op.entities.map((e) => e.name),
        auth: authEnabled && op.auth !== false,
        method: "POST",
        path: `/operations/${camelToKebabCase(op.name)}`,
      }),
    ),
    ...data.apis.map(
      (api): Endpoint => ({
        kind: "api",
        name: api.name,
        entities: api.entities.map((e) => e.name),
        auth: authEnabled && api.auth !== false,
        method: api.httpRoute.method.toUpperCase(),
        path: api.httpRoute.path,
      }),
    ),
    ...data.jobs.map(
      (job): Endpoint => ({
        kind: "job",
        name: job.name,
        entities: job.entities.map((e) => e.name),
        auth: false,
        schedule: job.schedule,
        method: "POST",
        path: `/__wasp/devtools/jobs/${job.name}/submit`,
      }),
    ),
  ];
}

// Same as `Wasp.Util.camelToKebabCase`, which names the operation routes.
export function camelToKebabCase(name: string): string {
  return name.replace(/([^A-Z])([A-Z])/g, "$1-$2").toLowerCase();
}

export async function fetchStudioData(): Promise<StudioData> {
  const response = await fetch(`${serverUrl}/__wasp/studio-data`);
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  return response.json();
}

// The app keeps its session in local storage, which this page shares since it
// runs on the same origin (see `@wasp/lib-sdk-core/browser`).
export function getAppSessionId(): string | null {
  try {
    const raw = localStorage.getItem("wasp:sessionId");
    return raw ? (JSON.parse(raw) as string) : null;
  } catch {
    return null;
  }
}

export interface CurrentUser {
  id: string;
  label: string;
}

export async function fetchCurrentUser(
  sessionId: string,
): Promise<CurrentUser | null> {
  const response = await fetch(`${serverUrl}/auth/me`, {
    headers: { Authorization: `Bearer ${sessionId}` },
  });
  if (!response.ok) return null;
  const body = await response.json();
  const user = body?.json ?? body;
  if (!user || typeof user !== "object") return null;
  const identities = Object.entries(user.identities ?? {}).find(([, v]) => v);
  const identity = identities?.[1] as { id?: string } | undefined;
  return {
    id: String(user.id),
    label: identity?.id ?? user.username ?? user.email ?? `User ${user.id}`,
  };
}

export interface SentRequest {
  method: string;
  url: string;
  headers: Record<string, string>;
  body: string | null;
}

export interface ReceivedResponse {
  request: SentRequest;
  status: number;
  statusText: string;
  durationMs: number;
  size: number;
  headers: Record<string, string>;
  bodyText: string;
  json: { ok: true; value: unknown } | { ok: false };
  at: number;
  networkError?: string;
}

export async function send(request: SentRequest): Promise<ReceivedResponse> {
  const start = performance.now();
  try {
    // Through the Vite dev server's proxy (see `../plugin.ts`), so APIs work
    // without CORS.
    const response = await fetch(toProxyUrl(request.url), {
      method: request.method,
      headers: request.headers,
      body: request.body ?? undefined,
    });
    const bodyText = await response.text();
    const durationMs = performance.now() - start;
    return {
      request,
      status: response.status,
      statusText: response.statusText,
      durationMs,
      size: new TextEncoder().encode(bodyText).length,
      headers: Object.fromEntries(response.headers.entries()),
      bodyText,
      json: parseJson(bodyText),
      at: Date.now(),
    };
  } catch (error) {
    return {
      request,
      status: 0,
      statusText: "Network error",
      durationMs: performance.now() - start,
      size: 0,
      headers: {},
      bodyText: "",
      json: { ok: false },
      at: Date.now(),
      networkError: error instanceof Error ? error.message : String(error),
    };
  }
}

function toProxyUrl(url: string): string {
  return url.startsWith(serverUrl)
    ? `/__wasp-devtools/playground/proxy${url.slice(serverUrl.length)}`
    : url;
}

export function parseJson(
  text: string,
): { ok: true; value: unknown } | { ok: false } {
  if (!text.trim()) return { ok: false };
  try {
    return { ok: true, value: JSON.parse(text) };
  } catch {
    return { ok: false };
  }
}

// Requests the server recorded (see `server/src/routes/devtools.ts`).
export interface RecordedRequest {
  id: number;
  startedAt: string;
  durationMs: number;
  method: string;
  path: string;
  query: Record<string, unknown>;
  status: number;
  requestHeaders: Record<string, string>;
  responseHeaders: Record<string, string>;
  requestBody: unknown;
  responseBody: string | null;
  responseSize: number;
  user: { id: string; identity: string | null } | null;
}

export async function fetchRecordedRequests(): Promise<RecordedRequest[]> {
  const response = await fetch(`${DEVTOOLS_ROUTE}/requests`);
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  return response.json();
}

export async function clearRecordedRequests(): Promise<void> {
  await fetch(`${DEVTOOLS_ROUTE}/requests`, { method: "DELETE" });
}

export function subscribeToRequests(
  onRequest: (request: RecordedRequest) => void,
  onStatus: (connected: boolean) => void,
): () => void {
  const source = new EventSource(`${DEVTOOLS_ROUTE}/requests/stream`);
  source.onopen = () => onStatus(true);
  source.onerror = () => onStatus(false);
  source.onmessage = (event) => onRequest(JSON.parse(event.data));
  return () => source.close();
}

export interface JobRun {
  id: string;
  state:
    | "created"
    | "retry"
    | "active"
    | "completed"
    | "expired"
    | "cancelled"
    | "failed";
  data: unknown;
  output: unknown;
  retrycount: number;
  createdon: string;
  startedon: string | null;
  completedon: string | null;
  startafter: string | null;
}

export async function fetchJobRuns(jobName: string): Promise<JobRun[]> {
  const response = await fetch(`${DEVTOOLS_ROUTE}/jobs/${jobName}/runs`);
  const body = await response.json();
  if (!response.ok) throw new Error(body?.message ?? `HTTP ${response.status}`);
  return body.runs;
}
