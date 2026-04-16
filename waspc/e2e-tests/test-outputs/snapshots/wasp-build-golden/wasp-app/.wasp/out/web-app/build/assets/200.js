const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/MainPage.js","assets/MainPage.css"])))=>i.map(i=>d[i]);
import { jsx, jsxs } from "react/jsx-runtime";
import { useState, useEffect, StrictMode, use, lazy, startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { useRouteError, createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";
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
function useIsClient() {
  const [isClient, setIsClient] = useState(false);
  useEffect(() => {
    setIsClient(true);
  }, []);
  return isClient;
}
function Layout({ children, isFallbackPage: isFallbackPage2 = false, clientEntrySrc }) {
  const isClient = useIsClient();
  const shouldRenderChildren = isClient || !isFallbackPage2;
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
const defaultQueryClientConfig = {};
function initializeQueryClient() {
  return new QueryClient(defaultQueryClientConfig);
}
const queryClientPromise = Promise.resolve(initializeQueryClient());
function WaspApp({ children }) {
  const queryClient = use(queryClientPromise);
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
  RootRoute: {
    Component: lazy(
      () => __vitePreload(() => import("./MainPage.js"), true ? __vite__mapDeps([0,1]) : void 0).then((m) => m.MainPage).then((component) => ({ default: component }))
    )
  }
};
const rootElement = void 0;
const routeObjects = getRouteObjects({
  routesMapping,
  rootElement
});
const router = createBrowserRouter(routeObjects, {
  basename: "/",
  // React Router will put hydration data on this property of the `window` object.
  // https://reactrouter.com/7.13.1/start/data/custom#4-hydrate-in-the-browser
  hydrationData: window.__staticRouterHydrationData
});
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {};
function App() {
  return /* @__PURE__ */ jsx(Layout, { isFallbackPage, children: /* @__PURE__ */ jsx(WaspApp, { children: /* @__PURE__ */ jsx(RouterProvider, { router }) }) });
}
await queryClientPromise;
startTransition(() => {
  hydrateRoot(document, /* @__PURE__ */ jsx(App, {}));
});
