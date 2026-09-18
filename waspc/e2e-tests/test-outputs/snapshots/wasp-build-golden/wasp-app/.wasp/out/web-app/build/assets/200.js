const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/MainPage.js","assets/MainPage.css"])))=>i.map(i=>d[i]);
import { StrictMode, startTransition, use, useSyncExternalStore } from "react";
import { hydrateRoot } from "react-dom/client";
import { Outlet, createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";
import { jsx, jsxs } from "react/jsx-runtime";
import { QueryClientProvider } from "@tanstack/react-query";
import { DefaultRootErrorBoundary, clearSessionId, getSessionId, initializeQueryClient, queryClientInitialized } from "@wasp.sh/lib-sdk-core/browser";
import { ensureEnvSchema, interpolatePath, stripTrailingSlash } from "@wasp.sh/lib-sdk-core";
import ky from "ky";
import * as z from "zod";
import "superjson";
//#region \0vite/modulepreload-polyfill.js
(function polyfill() {
	const relList = document.createElement("link").relList;
	if (relList && relList.supports && relList.supports("modulepreload")) return;
	for (const link of document.querySelectorAll("link[rel=\"modulepreload\"]")) processPreload(link);
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
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/app/layout.jsx
function Layout({ children, isFallbackPage = false, clientEntrySrc }) {
	const shouldRenderAppContent = useShouldRenderAppContent(isFallbackPage);
	return /* @__PURE__ */ jsx(StrictMode, { children: /* @__PURE__ */ jsxs("html", {
		lang: "en",
		children: [/* @__PURE__ */ jsxs("head", { children: [
			/* @__PURE__ */ jsx("meta", { charSet: "utf-8" }),
			/* @__PURE__ */ jsx("meta", {
				name: "viewport",
				content: "minimum-scale=1, initial-scale=1, width=device-width, shrink-to-fit=no"
			}),
			/* @__PURE__ */ jsx("link", {
				rel: "icon",
				href: "/favicon.ico"
			}),
			/* @__PURE__ */ jsx("title", { children: "wasp-app" }),
			clientEntrySrc ? /* @__PURE__ */ jsx("script", {
				type: "module",
				src: clientEntrySrc
			}) : null
		] }), /* @__PURE__ */ jsxs("body", { children: [/* @__PURE__ */ jsx("noscript", { children: "You need to enable JavaScript to run this app." }), shouldRenderAppContent ? children : null] })]
	}) });
}
function useShouldRenderAppContent(isFallbackPage) {
	const getOnClient = () => true;
	const getOnServer = () => !isFallbackPage;
	return useSyncExternalStore(emptySubscribe, getOnClient, getOnServer);
}
function emptySubscribe() {
	const emptyUnsubscribe = () => {};
	return emptyUnsubscribe;
}
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/env/schema.js
var userClientEnvSchema = z.object({});
var serverUrlSchema = z.string({ error: "REACT_APP_API_URL is required" }).pipe(z.url({ error: "REACT_APP_API_URL must be a valid URL" }));
z.object({ "REACT_APP_API_URL": serverUrlSchema });
var waspClientEnvSchema = z.object({ "REACT_APP_API_URL": serverUrlSchema });
var clientEnvSchema = z.object({
	...userClientEnvSchema.shape,
	...waspClientEnvSchema.shape
});
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/env.js
var env = ensureEnvSchema({
	"BASE_URL": "/",
	"DEV": false,
	"MODE": "production",
	"PROD": true,
	"REACT_APP_API_URL": "http://localhost:3001",
	"SSR": false
}, clientEnvSchema);
var config = { apiUrl: stripTrailingSlash(env["REACT_APP_API_URL"]) };
ky.extend({
	prefix: config.apiUrl,
	hooks: {
		beforeRequest: [({ request }) => {
			const sessionId = getSessionId();
			if (sessionId !== null) request.headers.set("Authorization", `Bearer ${sessionId}`);
		}],
		afterResponse: [({ request, response }) => {
			if (response.status === 401) {
				if (getSessionIdFromAuthorizationHeader(request.headers.get("Authorization")) === getSessionId()) clearSessionId();
			}
		}]
	}
});
function getSessionIdFromAuthorizationHeader(header) {
	if (header && header.startsWith("Bearer ")) return header.substring(7);
	else return null;
}
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/app/components/WaspApp.jsx
function WaspApp({ children }) {
	const queryClient = use(queryClientInitialized);
	return /* @__PURE__ */ jsx(QueryClientProvider, {
		client: queryClient,
		children
	});
}
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/router/index.js
var routes = { RootRoute: {
	to: "/",
	build: (options) => interpolatePath("/", void 0, options?.search, options?.hash)
} };
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/app/router.jsx
function getRouteObjects({ routesMapping, rootElement }) {
	const waspDefinedRoutes = [];
	const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
		return {
			path: route.to,
			...routesMapping[routeKey]
		};
	});
	return [{
		path: "/",
		element: rootElement,
		ErrorBoundary: DefaultRootErrorBoundary,
		children: [...waspDefinedRoutes, ...userDefinedRoutes]
	}];
}
//#endregion
//#region \0vite/preload-helper.js
var scriptRel = "modulepreload";
var assetsURL = function(dep) {
	return "/" + dep;
};
var seen = {};
var __vitePreload = function preload(baseModule, deps, importerUrl) {
	let promise = Promise.resolve();
	if (deps && deps.length > 0) {
		const links = document.getElementsByTagName("link");
		const cspNonceMeta = document.querySelector("meta[property=csp-nonce]");
		const cspNonce = cspNonceMeta?.nonce || cspNonceMeta?.getAttribute("nonce");
		function allSettled(promises) {
			return Promise.all(promises.map((p) => Promise.resolve(p).then((value) => ({
				status: "fulfilled",
				value
			}), (reason) => ({
				status: "rejected",
				reason
			}))));
		}
		function importMetaResolve(specifier) {
			if (import.meta.resolve) return import.meta.resolve(specifier);
			return new URL(
				specifier,
				/** #__KEEP__ */
				import.meta.url
			).href;
		}
		promise = allSettled(deps.map((dep) => {
			dep = assetsURL(dep, importerUrl);
			dep = importMetaResolve(dep);
			if (dep in seen) return;
			seen[dep] = true;
			const isCss = dep.endsWith(".css");
			for (let i = links.length - 1; i >= 0; i--) {
				const link = links[i];
				if (link.href === dep && (!isCss || link.rel === "stylesheet")) return;
			}
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
		}).filter((p) => p !== void 0));
	}
	function handlePreloadError(err) {
		const e = new Event("vite:preloadError", { cancelable: true });
		e.payload = err;
		window.dispatchEvent(e);
		if (!e.defaultPrevented) throw err;
	}
	return promise.then((res) => {
		for (const item of res || []) {
			if (item.status !== "rejected") continue;
			handlePreloadError(item.reason);
		}
		return baseModule().catch(handlePreloadError);
	});
};
//#endregion
//#region routes.tsx
var routesMapping = { RootRoute: { lazy: async () => {
	return { Component: await __vitePreload(() => import("./MainPage.js").then((m) => m.MainPage), __vite__mapDeps([0,1])) };
} } };
initializeQueryClient();
var routeObjects = getRouteObjects({
	routesMapping,
	rootElement: /* @__PURE__ */ jsx("div", {
		id: "root",
		children: /* @__PURE__ */ jsx(Outlet, {})
	})
});
//#endregion
//#region client-entry.tsx
var router = createBrowserRouter(routeObjects, {
	basename: "/",
	hydrationData: window.__staticRouterHydrationData
});
var { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {};
var routerProviderPromise = waitForRouterInitialized(router).then(() => /* @__PURE__ */ jsx(RouterProvider, { router }));
var fullAppTree = /* @__PURE__ */ jsx(Layout, {
	isFallbackPage,
	children: /* @__PURE__ */ jsx(WaspApp, { children: routerProviderPromise })
});
startTransition(() => {
	hydrateRoot(document, fullAppTree);
});
async function waitForRouterInitialized(router) {
	if (router.state.initialized) return;
	return new Promise((resolve) => {
		const unsubscribe = router.subscribe(() => {
			if (router.state.initialized) {
				unsubscribe();
				resolve();
			}
		});
	});
}
//#endregion
