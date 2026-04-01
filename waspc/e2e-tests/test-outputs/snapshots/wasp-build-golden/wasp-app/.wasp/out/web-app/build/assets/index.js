const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/MainPage.js","assets/MainPage.css"])))=>i.map(i=>d[i]);
import { jsx, jsxs } from "react/jsx-runtime";
import { useSyncExternalStore, StrictMode, use, startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { useRouteError, createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import ky from "ky";
import * as z from "zod";
import mitt from "mitt";
import "superjson";
(function polyfill() {
  const relList = document.createElement("link").relList;
  if (relList && relList.supports && relList.supports("modulepreload")) return;
  for (const link of document.querySelectorAll('link[rel="modulepreload"]')) processPreload(link);
  new MutationObserver((mutations) => {
    for (const mutation of mutations) {
      if (mutation.type !== "childList") continue;
      for (const node of mutation.addedNodes) if (node.tagName === "LINK" && node.rel === "modulepreload") processPreload(node);
    }
  }).observe(document, {
    childList: true,
    subtree: true
  });
  function getFetchOpts(link) {
    const fetchOpts = {};
    if (link.integrity) fetchOpts.integrity = link.integrity;
    if (link.referrerPolicy) fetchOpts.referrerPolicy = link.referrerPolicy;
    if (link.crossOrigin === "use-credentials") fetchOpts.credentials = "include";
    else if (link.crossOrigin === "anonymous") fetchOpts.credentials = "omit";
    else fetchOpts.credentials = "same-origin";
    return fetchOpts;
  }
  function processPreload(link) {
    if (link.ep) return;
    link.ep = true;
    const fetchOpts = getFetchOpts(link);
    fetch(link.href, fetchOpts);
  }
})();
function useIsClient() {
  return useSyncExternalStore(emptySubscribe, getClientValue, getServerValue);
}
function emptySubscribe() {
  return emptyUnsubscribe;
}
function emptyUnsubscribe() {
}
function getClientValue() {
  return true;
}
function getServerValue() {
  return false;
}
function Layout({ children, isFallbackPage = false, clientEntrySrc }) {
  const isClient = useIsClient();
  const shouldRenderChildren = isClient || !isFallbackPage;
  return /* @__PURE__ */ jsx(StrictMode, { children: /* @__PURE__ */ jsxs("html", { lang: "en", children: [
    /* @__PURE__ */ jsxs("head", { children: [
      /* @__PURE__ */ jsx("meta", { charSet: "utf-8" }),
      /* @__PURE__ */ jsx("meta", { name: "viewport", content: "minimum-scale=1, initial-scale=1, width=device-width, shrink-to-fit=no" }),
      /* @__PURE__ */ jsx("link", { rel: "icon", href: "/favicon.ico" }),
      /* @__PURE__ */ jsx("title", { children: "wasp-app" })
    ] }),
    /* @__PURE__ */ jsxs("body", { children: [
      /* @__PURE__ */ jsx("noscript", { children: "You need to enable JavaScript to run this app." }),
      /* @__PURE__ */ jsx("div", { id: "root", children: shouldRenderChildren ? children : null }),
      // We pass that argument in SSR builds and not in client builds.
      // This would usually cause a hydration mismatch, but React has an
      // exception for `<script>` tags, for this specific usecase, so it
      // will work fine.
      clientEntrySrc ? (
        // We'd usually use React prerender's `bootstrapModules` options for
        // injecting this script, but it would also add a `<link
        // rel="modulepreload">` tag that Vite doesn't handle correctly. So
        // we just add the script ourselves in the regular way.
        //
        // https://react.dev/reference/react-dom/static/prerenderToNodeStream
        /* @__PURE__ */ jsx(
          "script",
          {
            type: "module",
            src: clientEntrySrc,
            async: true
          }
        )
      ) : null
    ] })
  ] }) });
}
function stripTrailingSlash(url) {
  return url?.replace(/\/$/, "");
}
var define_process_env_default = {};
function colorize(color, text) {
  if (!supportsAnsiFormatting()) {
    return text;
  }
  const ansiColorCode = ansiColorCodes[color];
  return text.split("\n").map((line) => `${ansiColorCode}${line}${ansiResetCode}`).join("\n");
}
function supportsAnsiFormatting() {
  const isBrowser = !!globalThis.window;
  const isNode = !!globalThis.process;
  if (isBrowser && "chrome" in window) {
    return true;
  }
  if (isNode) {
    if ("NO_COLOR" in define_process_env_default) {
      return false;
    }
    return true;
  }
  return false;
}
const ansiColorCodes = {
  red: "\x1B[31m",
  yellow: "\x1B[33m"
};
const ansiResetCode = "\x1B[0m";
function ensureEnvSchema(data, schema) {
  const result = getValidatedEnvOrError(data, schema);
  if (result.success) {
    return result.data;
  } else {
    console.error(colorize("red", formatZodEnvError(result.error)));
    throw new Error("Error parsing environment variables");
  }
}
function getValidatedEnvOrError(env2, schema) {
  return schema.safeParse(env2);
}
function formatZodEnvError(error) {
  const flattenedIssues = z.flattenError(error);
  return [
    "══ Env vars validation failed ══",
    "",
    // Top-level errors
    ...flattenedIssues.formErrors,
    "",
    // Errors per field
    ...Object.entries(flattenedIssues.fieldErrors).map(([prop, error2]) => `${prop} - ${error2}`),
    "",
    "════════════════════════════════"
  ].join("\n");
}
const userClientEnvSchema = z.object({});
const serverUrlSchema = z.string({
  error: "REACT_APP_API_URL is required"
}).pipe(z.url({
  error: "REACT_APP_API_URL must be a valid URL"
}));
z.object({
  "REACT_APP_API_URL": serverUrlSchema.default("http://localhost:3001")
});
const waspProdClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema
});
function getClientEnvSchema(mode) {
  const waspClientEnvSchema = waspProdClientEnvSchema;
  return z.object({ ...userClientEnvSchema.shape, ...waspClientEnvSchema.shape });
}
const __vite_import_meta_env__ = { "BASE_URL": "/", "DEV": false, "MODE": "production", "PROD": true, "REACT_APP_API_URL": "http://localhost:3001", "SSR": false };
const env = ensureEnvSchema(__vite_import_meta_env__, getClientEnvSchema());
const apiUrl = stripTrailingSlash(env["REACT_APP_API_URL"]);
const config = {
  apiUrl
};
var HttpMethod;
(function(HttpMethod2) {
  HttpMethod2["Get"] = "GET";
  HttpMethod2["Post"] = "POST";
  HttpMethod2["Put"] = "PUT";
  HttpMethod2["Delete"] = "DELETE";
})(HttpMethod || (HttpMethod = {}));
const createStorage = typeof window === "undefined" || !window.localStorage ? createMemoryDataStore : createLocalStorageDataStore;
const storage = createStorage("wasp");
function createMemoryDataStore(prefix) {
  const store = /* @__PURE__ */ new Map();
  function getPrefixedKey(key) {
    return `${prefix}:${key}`;
  }
  return {
    getPrefixedKey,
    set(key, value) {
      store.set(getPrefixedKey(key), value);
    },
    get(key) {
      return store.get(getPrefixedKey(key));
    },
    remove(key) {
      store.delete(getPrefixedKey(key));
    },
    clear() {
      store.clear();
    }
  };
}
function createLocalStorageDataStore(prefix) {
  if (!window.localStorage) {
    throw new Error("Local storage is not available.");
  }
  function getPrefixedKey(key) {
    return `${prefix}:${key}`;
  }
  return {
    getPrefixedKey,
    set(key, value) {
      localStorage.setItem(getPrefixedKey(key), JSON.stringify(value));
    },
    get(key) {
      const value = localStorage.getItem(getPrefixedKey(key));
      try {
        return value ? JSON.parse(value) : void 0;
      } catch (e) {
        return void 0;
      }
    },
    remove(key) {
      localStorage.removeItem(getPrefixedKey(key));
    },
    clear() {
      Object.keys(localStorage).forEach((key) => {
        if (key.startsWith(prefix)) {
          localStorage.removeItem(key);
        }
      });
    }
  };
}
const apiEventsEmitter = mitt();
const WASP_APP_AUTH_SESSION_ID_NAME = "sessionId";
function getSessionId() {
  const sessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME);
  return sessionId ?? null;
}
function clearSessionId() {
  storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
  apiEventsEmitter.emit("sessionId.clear");
}
ky.extend({
  prefix: config.apiUrl,
  hooks: {
    beforeRequest: [
      ({ request }) => {
        const sessionId = getSessionId();
        if (sessionId !== null) {
          request.headers.set("Authorization", `Bearer ${sessionId}`);
        }
      }
    ],
    afterResponse: [
      ({ request, response }) => {
        if (response.status === 401) {
          const failingSessionId = getSessionIdFromAuthorizationHeader(request.headers.get("Authorization"));
          const currentSessionId = getSessionId();
          if (failingSessionId === currentSessionId) {
            clearSessionId();
          }
        }
      }
    ]
  }
});
if (typeof window !== "undefined") {
  window.addEventListener("storage", (event) => {
    if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) {
      if (!!event.newValue) {
        apiEventsEmitter.emit("sessionId.set");
      } else {
        apiEventsEmitter.emit("sessionId.clear");
      }
    }
  });
}
function getSessionIdFromAuthorizationHeader(header) {
  const prefix = "Bearer ";
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length);
  } else {
    return null;
  }
}
const defaultQueryClientConfig = {};
let resolveQueryClientInitialized;
const queryClientInitialized = new Promise((resolve) => {
  resolveQueryClientInitialized = resolve;
});
function initializeQueryClient() {
  const queryClient = new QueryClient(defaultQueryClientConfig);
  resolveQueryClientInitialized(queryClient);
}
function WaspApp({ children }) {
  const queryClient = use(queryClientInitialized);
  return /* @__PURE__ */ jsx(QueryClientProvider, { client: queryClient, children });
}
const scriptRel = "modulepreload";
const assetsURL = function(dep) {
  return "/" + dep;
};
const seen = {};
const __vitePreload = function preload(baseModule, deps, importerUrl) {
  let promise = Promise.resolve();
  if (deps && deps.length > 0) {
    let allSettled = function(promises$2) {
      return Promise.all(promises$2.map((p) => Promise.resolve(p).then((value$1) => ({
        status: "fulfilled",
        value: value$1
      }), (reason) => ({
        status: "rejected",
        reason
      }))));
    };
    document.getElementsByTagName("link");
    const cspNonceMeta = document.querySelector("meta[property=csp-nonce]");
    const cspNonce = cspNonceMeta?.nonce || cspNonceMeta?.getAttribute("nonce");
    promise = allSettled(deps.map((dep) => {
      dep = assetsURL(dep);
      if (dep in seen) return;
      seen[dep] = true;
      const isCss = dep.endsWith(".css");
      const cssSelector = isCss ? '[rel="stylesheet"]' : "";
      if (document.querySelector(`link[href="${dep}"]${cssSelector}`)) return;
      const link = document.createElement("link");
      link.rel = isCss ? "stylesheet" : scriptRel;
      if (!isCss) link.as = "script";
      link.crossOrigin = "";
      link.href = dep;
      if (cspNonce) link.setAttribute("nonce", cspNonce);
      document.head.appendChild(link);
      if (isCss) return new Promise((res, rej) => {
        link.addEventListener("load", res);
        link.addEventListener("error", () => rej(/* @__PURE__ */ new Error(`Unable to preload CSS for ${dep}`)));
      });
    }));
  }
  function handlePreloadError(err$2) {
    const e$1 = new Event("vite:preloadError", { cancelable: true });
    e$1.payload = err$2;
    window.dispatchEvent(e$1);
    if (!e$1.defaultPrevented) throw err$2;
  }
  return promise.then((res) => {
    for (const item of res || []) {
      if (item.status !== "rejected") continue;
      handlePreloadError(item.reason);
    }
    return baseModule().catch(handlePreloadError);
  });
};
const wrapperStyles = {
  display: "flex",
  minHeight: "80vh",
  justifyContent: "center",
  alignItems: "center"
};
function FullPageWrapper({ children, className }) {
  const classNameWithDefaults = ["wasp-full-page-wrapper", className].filter(Boolean).join(" ");
  return /* @__PURE__ */ jsx("div", { className: classNameWithDefaults, style: wrapperStyles, children });
}
function DefaultRootErrorBoundary() {
  const error = useRouteError();
  console.error(error);
  return /* @__PURE__ */ jsx(FullPageWrapper, { children: /* @__PURE__ */ jsx("div", { children: "There was an error rendering this page. Check the browser console for more information." }) });
}
function interpolatePath(path, params, search, hash) {
  const interpolatedPath = path;
  const interpolatedSearch = search ? `?${new URLSearchParams(search).toString()}` : "";
  const interpolatedHash = hash ? `#${hash}` : "";
  return interpolatedPath + interpolatedSearch + interpolatedHash;
}
const routes = {
  RootRoute: {
    to: "/",
    build: (options) => interpolatePath("/", void 0, options?.search, options?.hash)
  }
};
function getRouteObjects({ routesMapping: routesMapping2, rootElement: rootElement2 }) {
  const waspDefinedRoutes = [];
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    return {
      path: route.to,
      ...routesMapping2[routeKey]
    };
  });
  return [{
    path: "/",
    element: rootElement2,
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes
    ]
  }];
}
const routesMapping = {
  RootRoute: { lazy: async () => {
    const Component = await __vitePreload(() => import("./MainPage.js"), true ? __vite__mapDeps([0,1]) : void 0).then((m) => m.MainPage);
    return { Component };
  } }
};
initializeQueryClient();
const rootElement = void 0;
const routeObjects = getRouteObjects({
  routesMapping,
  rootElement
});
const hydrationData = window.__staticRouterHydrationData;
const router = createBrowserRouter(routeObjects, {
  basename: "/",
  hydrationData
});
function App({ isFallbackPage }) {
  return /* @__PURE__ */ jsx(Layout, { isFallbackPage, children: /* @__PURE__ */ jsx(WaspApp, { children: /* @__PURE__ */ jsx(RouterProvider, { router }) }) });
}
startTransition(() => {
  const isFallbackpage = hydrationData == null;
  hydrateRoot(document, /* @__PURE__ */ jsx(App, { isFallbackPage: isFallbackpage }));
});
