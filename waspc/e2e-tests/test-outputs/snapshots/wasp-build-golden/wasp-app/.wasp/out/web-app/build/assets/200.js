const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/MainPage.js","assets/MainPage.css"])))=>i.map(i=>d[i]);
import { StrictMode, forwardRef, startTransition, use, useSyncExternalStore } from "react";
import { hydrateRoot } from "react-dom/client";
import { Outlet, createBrowserRouter, useRouteError } from "react-router";
import { RouterProvider } from "react-router/dom";
import { jsx, jsxs } from "react/jsx-runtime";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import "@wasp.sh/lib-auth";
import mitt from "mitt";
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
//#region node_modules/@wasp.sh/lib-sdk-core/dist/resources-BaMGi48Q.js
var defaultQueryClientConfig = {};
var resolveQueryClientInitialized;
var queryClientInitialized = new Promise((resolve) => {
	resolveQueryClientInitialized = resolve;
});
function initializeQueryClient() {
	const queryClient = new QueryClient(defaultQueryClientConfig);
	resolveQueryClientInitialized(queryClient);
}
//#endregion
//#region node_modules/@wasp.sh/lib-sdk-core/dist/browser.js
var wrapperStyles = {
	display: "flex",
	minHeight: "80vh",
	justifyContent: "center",
	alignItems: "center"
};
function FullPageWrapper({ children, className }) {
	return /* @__PURE__ */ jsx("div", {
		className: ["wasp-full-page-wrapper", className].filter(Boolean).join(" "),
		style: wrapperStyles,
		children
	});
}
function DefaultRootErrorBoundary() {
	const error = useRouteError();
	console.error(error);
	return /* @__PURE__ */ jsx(FullPageWrapper, { children: /* @__PURE__ */ jsx("div", { children: "There was an error rendering this page. Check the browser console for more information." }) });
}
var commonMessageStyles = {
	borderRadius: ".5rem",
	padding: "1rem"
};
({ ...commonMessageStyles });
({ ...commonMessageStyles });
var apiEventsEmitter = mitt();
var Form_module_default = {
	"form": "VbuAUq_form",
	"formError": "VbuAUq_formError",
	"formInput": "VbuAUq_formInput",
	"formItemGroup": "VbuAUq_formItemGroup",
	"formLabel": "VbuAUq_formLabel",
	"formTextarea": "VbuAUq_formTextarea",
	"submitButton": "VbuAUq_submitButton"
};
var clsx = (...classes) => {
	return classes.filter(Boolean).join(" ");
};
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("form", {
	className: clsx(Form_module_default.form, className),
	...props,
	ref,
	children
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("div", {
	className: clsx(Form_module_default.formItemGroup, className),
	...props,
	ref,
	children
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("label", {
	className: clsx(Form_module_default.formLabel, className),
	...props,
	ref,
	children
}));
forwardRef(({ className, ...props }, ref) => /* @__PURE__ */ jsx("input", {
	className: clsx(Form_module_default.formInput, className),
	...props,
	ref
}));
forwardRef(({ className, ...props }, ref) => /* @__PURE__ */ jsx("textarea", {
	className: clsx(Form_module_default.formTextarea, className),
	...props,
	ref
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("div", {
	className: clsx(Form_module_default.formError, className),
	...props,
	ref,
	children
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("button", {
	className: clsx(Form_module_default.submitButton, className),
	...props,
	ref,
	children
}));
var Message_module_default = {
	"message": "vbkyMW_message",
	"messageError": "vbkyMW_messageError",
	"messageSuccess": "vbkyMW_messageSuccess"
};
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("div", {
	className: clsx(Message_module_default.message, className),
	...props,
	ref,
	children
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("div", {
	className: clsx(Message_module_default.messageError, className),
	...props,
	ref,
	children
}));
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("div", {
	className: clsx(Message_module_default.messageSuccess, className),
	...props,
	ref,
	children
}));
var SocialButton_module_default = { "socialButton": "Liyq9q_socialButton" };
forwardRef(({ children, className, ...props }, ref) => /* @__PURE__ */ jsx("a", {
	className: clsx(SocialButton_module_default.socialButton, className),
	...props,
	ref,
	children
}));
var storage = (typeof window === "undefined" || !window.localStorage ? createMemoryDataStore : createLocalStorageDataStore)("wasp");
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
	if (!window.localStorage) throw new Error("Local storage is not available.");
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
				return;
			}
		},
		remove(key) {
			localStorage.removeItem(getPrefixedKey(key));
		},
		clear() {
			Object.keys(localStorage).forEach((key) => {
				if (key.startsWith(prefix)) localStorage.removeItem(key);
			});
		}
	};
}
var WASP_APP_AUTH_SESSION_ID_NAME = "sessionId";
function getSessionId() {
	return storage.get(WASP_APP_AUTH_SESSION_ID_NAME) ?? null;
}
function clearSessionId() {
	storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
	apiEventsEmitter.emit("sessionId.clear");
}
if (typeof window !== "undefined") window.addEventListener("storage", (event) => {
	if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) if (!!event.newValue) apiEventsEmitter.emit("sessionId.set");
	else apiEventsEmitter.emit("sessionId.clear");
});
z.object({ sessionId: z.string() });
z.object({
	success: z.boolean(),
	reason: z.string().optional()
});
/**
* Wraps each line of text with ANSI color codes.
* Only works in Node.js (server-side), not in the browser.
*
* Each line is individually wrapped because Wasp reads child process
* output line-by-line and re-prints it with a prefix (e.g. `[ Server ]`).
* A single color code spanning multiple lines would only color the first line.
*
* @example
* ```typescript
* console.log(colorize('red', 'This is red text'));
* ```
*/
function colorize(color, text) {
	if (!supportsAnsiFormatting()) return text;
	const ansiColorCode = ansiColorCodes[color];
	return text.split("\n").map((line) => `${ansiColorCode}${line}${ansiResetCode}`).join("\n");
}
function supportsAnsiFormatting() {
	const isBrowser = !!globalThis.window;
	const isNode = !!globalThis.process;
	if (isBrowser && "chrome" in window) return true;
	if (isNode) {
		if ("NO_COLOR" in {}) return false;
		return true;
	}
	return false;
}
var ansiColorCodes = {
	red: "\x1B[31m",
	yellow: "\x1B[33m"
};
var ansiResetCode = "\x1B[0m";
function ensureEnvSchema(data, schema) {
	const result = getValidatedEnvOrError(data, schema);
	if (result.success) return result.data;
	else {
		console.error(colorize("red", formatZodEnvError(result.error)));
		throw new Error("Error parsing environment variables");
	}
}
function getValidatedEnvOrError(env, schema) {
	return schema.safeParse(env);
}
function formatZodEnvError(error) {
	const flattenedIssues = z.flattenError(error);
	return [
		"══ Env vars validation failed ══",
		"",
		...flattenedIssues.formErrors,
		"",
		...Object.entries(flattenedIssues.fieldErrors).map(([prop, error]) => `${prop} - ${error}`),
		"",
		"════════════════════════════════"
	].join("\n");
}
function interpolatePath(path, params, search, hash) {
	const interpolatedPath = params ? interpolatePathParams(path, params) : path;
	const interpolatedSearch = search ? `?${new URLSearchParams(search).toString()}` : "";
	const interpolatedHash = hash ? `#${hash}` : "";
	return interpolatedPath + interpolatedSearch + interpolatedHash;
}
function interpolatePathParams(path, params) {
	function mapPathPart(part) {
		if (part === "*") return params["*"];
		if (part.startsWith(":")) return params[extractParamNameFromPathPart(part)];
		return part;
	}
	const interpolatedPath = path.split("/").map(mapPathPart).filter(isValidPathPart).join("/");
	return path.startsWith("/") ? `/${interpolatedPath}` : interpolatedPath;
}
function isValidPathPart(part) {
	return !!part;
}
function extractParamNameFromPathPart(paramString) {
	if (paramString.endsWith("?")) return paramString.slice(1, -1);
	return paramString.slice(1);
}
function stripTrailingSlash(url) {
	return url?.replace(/\/$/, "");
}
//#endregion
//#region .wasp/out/sdk/wasp/dist/client/env/schema.js
var userClientEnvSchema = z.object({});
var serverUrlSchema = z.string({ error: "REACT_APP_API_URL is required" }).pipe(z.url({ error: "REACT_APP_API_URL must be a valid URL" }));
z.object({ "REACT_APP_API_URL": serverUrlSchema });
var waspClientEnvSchema = z.object({ "REACT_APP_API_URL": serverUrlSchema });
var config = { apiUrl: stripTrailingSlash(ensureEnvSchema({
	"BASE_URL": "/",
	"DEV": false,
	"MODE": "production",
	"PROD": true,
	"REACT_APP_API_URL": "http://localhost:3001",
	"SSR": false
}, z.object({
	...userClientEnvSchema.shape,
	...waspClientEnvSchema.shape
}))["REACT_APP_API_URL"]) };
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
