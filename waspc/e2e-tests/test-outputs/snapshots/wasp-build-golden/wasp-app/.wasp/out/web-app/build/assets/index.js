import { jsx, jsxs } from "react/jsx-runtime";
import * as React from "react";
import * as ReactDOM from "react-dom/client";
import { useRouteError, createBrowserRouter, RouterProvider, Outlet } from "react-router";
import axios from "axios";
import * as z from "zod";
import mitt from "mitt";
import "superjson";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
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
function createLocalStorageDataStore(prefix) {
  function getPrefixedKey(key) {
    return `${prefix}:${key}`;
  }
  return {
    getPrefixedKey,
    set(key, value) {
      ensureLocalStorageIsAvailable();
      localStorage.setItem(getPrefixedKey(key), JSON.stringify(value));
    },
    get(key) {
      ensureLocalStorageIsAvailable();
      const value = localStorage.getItem(getPrefixedKey(key));
      try {
        return value ? JSON.parse(value) : void 0;
      } catch (e) {
        return void 0;
      }
    },
    remove(key) {
      ensureLocalStorageIsAvailable();
      localStorage.removeItem(getPrefixedKey(key));
    },
    clear() {
      ensureLocalStorageIsAvailable();
      Object.keys(localStorage).forEach((key) => {
        if (key.startsWith(prefix)) {
          localStorage.removeItem(key);
        }
      });
    }
  };
}
const storage = createLocalStorageDataStore("wasp");
function ensureLocalStorageIsAvailable() {
  if (!window.localStorage) {
    throw new Error("Local storage is not available.");
  }
}
const apiEventsEmitter = mitt();
const api = axios.create({
  baseURL: config.apiUrl
});
const WASP_APP_AUTH_SESSION_ID_NAME = "sessionId";
function getSessionId() {
  const sessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME);
  return sessionId ?? null;
}
function clearSessionId() {
  storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
  apiEventsEmitter.emit("sessionId.clear");
}
api.interceptors.request.use((config2) => {
  const sessionId = getSessionId();
  if (sessionId !== null) {
    config2.headers["Authorization"] = `Bearer ${sessionId}`;
  }
  return config2;
});
api.interceptors.response.use(void 0, (error) => {
  const failingSessionId = getSessionIdFromAuthorizationHeader(error.config.headers["Authorization"]);
  const currentSessionId = getSessionId();
  if (error.response?.status === 401 && failingSessionId === currentSessionId) {
    clearSessionId();
  }
  return Promise.reject(error);
});
window.addEventListener("storage", (event) => {
  if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) {
    if (!!event.newValue) {
      apiEventsEmitter.emit("sessionId.set");
    } else {
      apiEventsEmitter.emit("sessionId.clear");
    }
  }
});
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
function getRouter({ routesMapping: routesMapping2, rootElement }) {
  const waspDefinedRoutes = [];
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    return {
      path: route.to,
      Component: routesMapping2[routeKey]
    };
  });
  const browserRouter = createBrowserRouter([{
    path: "/",
    element: rootElement,
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes
    ]
  }], {
    basename: "/"
  });
  return /* @__PURE__ */ jsx(RouterProvider, { router: browserRouter });
}
function WaspApp({ rootElement, routesMapping: routesMapping2 }) {
  const [queryClient, setQueryClient] = React.useState(null);
  React.useEffect(() => {
    queryClientInitialized.then(setQueryClient);
  }, []);
  if (!queryClient) {
    return null;
  }
  const router = getRouter({
    rootElement,
    routesMapping: routesMapping2
  });
  return /* @__PURE__ */ jsx(QueryClientProvider, { client: queryClient, children: router });
}
const DefaultRootComponent = () => /* @__PURE__ */ jsx(Outlet, {});
let isAppInitialized = false;
function getWaspApp({ rootElement = /* @__PURE__ */ jsx(DefaultRootComponent, {}), routesMapping: routesMapping2 }) {
  if (!isAppInitialized) {
    initializeQueryClient();
    isAppInitialized = true;
  }
  return /* @__PURE__ */ jsx(WaspApp, { rootElement, routesMapping: routesMapping2 });
}
const Logo = "data:image/svg+xml,%3csvg%20id='Layer_1'%20data-name='Layer%201'%20xmlns='http://www.w3.org/2000/svg'%20viewBox='0%200%20161%20161'%3e%3cdefs%3e%3cstyle%3e.cls-1{fill:%23f5cc05;}.cls-2{fill-rule:evenodd;}%3c/style%3e%3c/defs%3e%3cg%20id='Page-1'%3e%3cg%20id='Group-23'%3e%3ccircle%20id='Oval'%20class='cls-1'%20cx='80.5'%20cy='80.5'%20r='79'/%3e%3cg%20id='Group-36'%3e%3cg%20id='_'%20data-name='}'%3e%3cpath%20id='path-2'%20class='cls-2'%20d='M88.67,114.33h2.91q6,0,7.87-1.89c1.22-1.25,1.83-3.9,1.83-7.93V93.89c0-4.46.65-7.7,1.93-9.73s3.51-3.43,6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88H88.67V39.5h2.65q10.65,0,14.24,3.15t3.59,12.62V65.56c0,4.28.77,7.24,2.29,8.87s4.3,2.44,8.32,2.44h2.74V83h-2.74q-6,0-8.32,2.49c-1.52,1.65-2.29,4.64-2.29,9v10.25q0,9.47-3.59,12.64T91.32,120.5H88.67Z'/%3e%3cpath%20id='path-2-2'%20data-name='path-2'%20class='cls-2'%20d='M88.67,114.33h2.91q6,0,7.87-1.89c1.22-1.25,1.83-3.9,1.83-7.93V93.89c0-4.46.65-7.7,1.93-9.73s3.51-3.43,6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88H88.67V39.5h2.65q10.65,0,14.24,3.15t3.59,12.62V65.56c0,4.28.77,7.24,2.29,8.87s4.3,2.44,8.32,2.44h2.74V83h-2.74q-6,0-8.32,2.49c-1.52,1.65-2.29,4.64-2.29,9v10.25q0,9.47-3.59,12.64T91.32,120.5H88.67Z'/%3e%3c/g%3e%3cg%20id='text831'%3e%3cg%20id='_2'%20data-name='='%3e%3cpath%20id='path-3'%20class='cls-2'%20d='M38.5,85.15H75.83v7.58H38.5Zm0-17.88H75.83v7.49H38.5Z'/%3e%3cpath%20id='path-3-2'%20data-name='path-3'%20class='cls-2'%20d='M38.5,85.15H75.83v7.58H38.5Zm0-17.88H75.83v7.49H38.5Z'/%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/svg%3e";
function MainPage() {
  return /* @__PURE__ */ jsxs("main", { className: "container", children: [
    /* @__PURE__ */ jsx("img", { className: "logo", src: Logo, alt: "wasp" }),
    /* @__PURE__ */ jsx("h2", { className: "title", children: "Welcome to Wasp!" }),
    /* @__PURE__ */ jsxs("p", { className: "content", children: [
      "This is page ",
      /* @__PURE__ */ jsx("code", { children: "MainPage" }),
      " located at route ",
      /* @__PURE__ */ jsx("code", { children: "/" }),
      ".",
      /* @__PURE__ */ jsx("br", {}),
      "Open ",
      /* @__PURE__ */ jsx("code", { children: "src/MainPage.tsx" }),
      " to edit it."
    ] }),
    /* @__PURE__ */ jsxs("div", { className: "buttons", children: [
      /* @__PURE__ */ jsx(
        "a",
        {
          className: "button button-filled",
          href: "https://wasp.sh/docs/tutorial/create",
          target: "_blank",
          rel: "noreferrer noopener",
          children: "Take the Tutorial"
        }
      ),
      /* @__PURE__ */ jsx(
        "a",
        {
          className: "button button-outlined",
          href: "https://discord.com/invite/rzdnErX",
          target: "_blank",
          rel: "noreferrer noopener",
          children: "Chat on Discord"
        }
      )
    ] })
  ] });
}
const routesMapping = {
  RootRoute: MainPage
};
const app = getWaspApp({
  routesMapping
});
ReactDOM.createRoot(document.getElementById("root")).render(
  /* @__PURE__ */ jsx(React.StrictMode, { children: app })
);
