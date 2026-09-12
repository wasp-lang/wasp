{{={= =}=}}
/**
 * In the `single` deployment mode, the client and the server always share one origin.
 * In production, the server serves the client directly.
 * In development, the Vite dev server forwards the server's routes to the server URL
 * ({@link devProxyTarget}) through a proxy.
 */
import type { ConfigEnv, ProxyOptions } from "vite";
import type { HttpMethod } from "../../../index.js";

const devProxyTargetEnvVarName = "{= devProxyTargetEnvVarName =}";
const devProxyTarget = process.env[devProxyTargetEnvVarName];

/**
 * Whether Vite is running as the app's dev server.
 *
 * `command` is also `"serve"` when:
 * - `vite preview` serves the built client.
 * - Vitest starts a dev server of its own.
 *
 * So we have to filter those cases out.
 */
export function isAppDevServer({
  command,
  mode,
  isPreview,
}: Pick<ConfigEnv, "command" | "mode" | "isPreview">): boolean {
  return command === "serve" && !isPreview && mode !== "test";
}

export function throwIfDevProxyTargetMissing(): void {
  if (devProxyTarget === undefined) {
    throw new Error(
      `The environment variable "${devProxyTargetEnvVarName}" is not set. ` +
        "It is required for the dev server to work properly in `single` deployment mode.\n" +
        "Please run the app with `wasp start`, which sets it automatically, or if you are starting the " +
        "dev server yourself, set it to the Wasp server's URL.",
    );
  }
}

/**
 * Returns a proxy configuration if the current environment is the
 * dev environment for a Wasp app.
 *
 * Other Wasp plugins (e.g. `wasp:validate-env`) create throwaway
 * middleware-mode servers during `vite build`, which also report `serve`,
 * but don't have the dev proxy target env var set.
 */
export function makeDevServerProxy(
  configEnv: ConfigEnv,
): Record<string, ProxyOptions> | undefined {
  if (!isAppDevServer(configEnv) || devProxyTarget === undefined) {
    return undefined;
  }

  return Object.fromEntries([
    ...proxiedExactPaths.map((proxiedPath) => [
      makeExactProxyRegex(proxiedPath),
      makeProxyOptions(devProxyTarget, proxiedPath),
    ]),
    ...proxiedSubtreePaths.map((proxiedPath) => [
      makeSubtreeProxyRegex(proxiedPath),
      makeProxyOptions(devProxyTarget, proxiedPath),
    ]),
  ]);
}

type ProxiedPath = { path: string; httpMethods: ProxiedHttpMethods };
type ProxiedHttpMethods = HttpMethodName[] | typeof allHttpMethods;
type HttpMethodName = `${HttpMethod}`;
const allHttpMethods = "ALL";

/** Server routes we can name in full, so the proxy forwards only these exact paths. */
const proxiedExactPaths: ProxiedPath[] = {=& proxiedExactPaths =};
/**
 * Server routes we can only name by their static beginning, so the proxy forwards
 * everything under them. E.g. `/some-path/:id` proxies the whole `/some-path` subtree.
 */
const proxiedSubtreePaths: ProxiedPath[] = {=& proxiedSubtreePaths =};

/**
 * Vite treats plain string keys as `startsWith` matches (e.g. "/api"
 * would match an "/apis" page), so we turn them into anchored regexes.
 */
function makeExactProxyRegex({ path }: ProxiedPath): string {
  // An optional trailing slash, then the query or the url's end.
  const restOfExactPath = String.raw`/?(?:\?|$)`;

  return `^${pathToProxyRegex(path)}${restOfExactPath}`;
}

function makeSubtreeProxyRegex({ path }: ProxiedPath): string {
  // The root subtree is every path.
  if (path === "/") {
    return "^/";
  }

  // A slash starting a child segment, or the query, or the url's end.
  const restOfSubtreePath = String.raw`(?:/|\?|$)`;
  return `^${pathToProxyRegex(path)}${restOfSubtreePath}`;
}

/**
 * Express matches routes case-insensitively, so the proxy has to as well.
 * Vite sets no regex flags, so we have to do case-insensitivity manually.
 */
function pathToProxyRegex(path: string): string {
  return escapeRegex(path).replace(
    /[a-zA-Z]/g,
    (letter) => `[${letter.toLowerCase()}${letter.toUpperCase()}]`,
  );
}

function escapeRegex(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * The client handles the request only when:
 * - The request is either a GET or HEAD HTTP method.
 * - The request route is not already reserved by the server.
 *
 * In that case, we have to define a {@link ProxyOptions.bypass},
 * which lets the client handle the request instead.
 */
function makeProxyOptions(
  devProxyTarget: string,
  { httpMethods }: ProxiedPath,
): ProxyOptions {
  const proxyOptions: ProxyOptions = {
    target: devProxyTarget,
    changeOrigin: false,
    ws: true,
  };

  if (httpMethods === allHttpMethods) {
    return proxyOptions;
  }

  const proxiedHttpMethods = new Set<string>(httpMethods);
  return {
    ...proxyOptions,
    bypass: (req) =>
      shouldProxyRequest(req.method, proxiedHttpMethods) ? undefined : req.url,
  };
}

function shouldProxyRequest(
  requestHttpMethod: string | undefined,
  proxiedHttpMethods: Set<string>,
): boolean {
  if (requestHttpMethod === undefined) {
    return true;
  }
  if (requestHttpMethod !== "GET" && requestHttpMethod !== "HEAD") {
    return true;
  }
  return proxiedHttpMethods.has(
    // Wasp has no concept of HEAD server routes, and Express responds
    // to an unhandled HEAD request with the path's GET route.
    requestHttpMethod === "HEAD" ? "GET" : requestHttpMethod,
  );
}
