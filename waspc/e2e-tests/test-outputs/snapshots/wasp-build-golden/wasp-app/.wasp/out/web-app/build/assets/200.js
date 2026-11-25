const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/MainPage.js","assets/MainPage.css"])))=>i.map(i=>d[i]);
import { StrictMode, startTransition, use, useEffect, useSyncExternalStore } from "react";
import { hydrateRoot } from "react-dom/client";
import { Outlet, createBrowserRouter, useRouteError } from "react-router";
import { RouterProvider } from "react-router/dom";
import { jsx, jsxs } from "react/jsx-runtime";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import ky, { isHTTPError } from "ky";
import mitt from "mitt";
import * as z from "zod";
import "superjson";
// \0vite/modulepreload-polyfill.js
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
// .wasp/out/sdk/wasp/dist/client/app/layout.jsx
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
// node_modules/@wasp/lib-sdk-core/dist/HttpError.mjs
/**
* An error for an HTTP error response, which has a status code in the range
* `[400, 600)`.
*
* Its `status`, `message`, and `data` are part of the response, so only put
* information in them that is safe to show to the user.
*
* @typeParam Data The type of the error's `data`. It defaults to
* {@link HttpErrorData}, an object with any fields.
*
* @example
* ```ts
* import { HttpError } from "wasp/errors";
*
* export const getTask = async ({ id }, context) => {
*   if (!context.user) {
*     throw new HttpError(401, "You must be logged in.");
*   }
*   // ...
* };
* ```
*
* @example
* Sending extra information and response headers:
*
* ```ts
* import { HttpError } from "wasp/errors";
*
* throw new HttpError(429, "Too many requests", {
*   data: { limitPerMinute: 60 },
*   headers: { "Retry-After": "60" },
* });
* ```
*
* @example
* Reading it on the client:
*
* ```ts
* import { createTask } from "wasp/client/operations";
* import { HttpError } from "wasp/errors";
*
* try {
*   await createTask({ description: "Buy milk" });
* } catch (error: unknown) {
*   if (error instanceof HttpError && error.status === 429) {
*     const retryAfter = error.response.headers.get("Retry-After");
*   }
* }
* ```
*
* @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
*/
var HttpError = class extends Error {
	/**
	* The HTTP response the error stands for. Read the status code and the
	* headers from it.
	*
	* Don't read its body, since it may already be consumed. Use `message` and
	* `data` instead.
	*/
	response;
	/**
	* Extra information about the error, e.g. which form field is invalid.
	* It is `undefined` if the error was created without it.
	*/
	data;
	/**
	* The HTTP status code of the response, an integer in the range
	* `[400, 600)`.
	*/
	get status() {
		return this.response.status;
	}
	/**
	* @deprecated Use {@link HttpError.status | status} instead.
	*/
	get statusCode() {
		return this.status;
	}
	constructor(statusOrResponse, message, options = {}) {
		super(message, options);
		if (Error.captureStackTrace) Error.captureStackTrace(this, new.target);
		this.name = "HttpError";
		const status = typeof statusOrResponse === "number" ? statusOrResponse : statusOrResponse.status;
		if (!isHttpErrorStatusCode(status)) throw new Error("The status of an HttpError has to be an integer in the range [400, 600).");
		this.data = options.data;
		this.response = typeof statusOrResponse === "number" ? Response.json(toHttpErrorBody(this), {
			status,
			headers: options.headers
		}) : statusOrResponse;
	}
};
/**
* Checks that a value is a status code an {@link HttpError} accepts, which is
* an integer in the range `[400, 600)`.
*/
function isHttpErrorStatusCode(value) {
	return typeof value === "number" && Number.isInteger(value) && value >= 400 && value < 600;
}
/**
* Builds the JSON body of the response of an {@link HttpError}.
*/
function toHttpErrorBody(error) {
	return {
		message: error.message,
		data: error.data
	};
}
var handledErrors = /* @__PURE__ */ new WeakSet();
/**
* Reports a client error if it is unexpected, once per error object.
*
* An {@link HttpError} or ky's `HTTPError` with a status below 500 is
* expected, because it comes from an error response that the server sent on
* purpose.
*
* @param error The thrown value.
* @param context Where the error surfaced.
*/
function reportClientError(error, context) {
	if (isExpectedHttpError(error)) return;
	if (typeof error === "object" && error !== null) {
		if (handledErrors.has(error)) return;
		handledErrors.add(error);
	}
	reportUnexpectedClientError(error, context);
}
function isExpectedHttpError(error) {
	if (error instanceof HttpError) return error.status < 500;
	return isHTTPError(error) && error.response.status < 500;
}
/**
* Passes the errors that reach the window to {@link reportClientError}, e.g.
* errors thrown in event handlers and rejected promises that nobody awaited.
*
* @see https://developer.mozilla.org/en-US/docs/Web/API/Window/error_event
* @see https://developer.mozilla.org/en-US/docs/Web/API/Window/unhandledrejection_event
*/
function listenForUncaughtClientErrors() {
	window.addEventListener("error", (event) => {
		reportClientError(event.error ?? event.message, {
			source: "window",
			event: "error"
		});
	});
	window.addEventListener("unhandledrejection", (event) => {
		reportClientError(event.reason, {
			source: "window",
			event: "unhandledrejection"
		});
	});
}
/**
* Reports an error thrown while rendering. It has the shape of React's
* `onCaughtError` and `onUncaughtError` root options.
*
* @see https://react.dev/reference/react-dom/client/hydrateRoot#parameters
*/
function reportRenderError(error, { componentStack }) {
	reportClientError(error, {
		source: "render",
		componentStack
	});
}
function reportUnexpectedClientError(error, context) {
	console.error(`Unexpected error in ${context.source}:`, error);
	if (context.source === "render" && context.componentStack) console.error(`The error happened in:${context.componentStack}`);
	callClientOnErrorFn(error, context);
}
async function callClientOnErrorFn(error, context) {
	try {
		await void 0;
	} catch (onErrorFnError) {
		console.error("The `client.onError` function failed:", onErrorFnError);
	}
}
// node_modules/@wasp/lib-sdk-core/dist/browser/app/FullPageWrapper.mjs
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
// node_modules/@wasp/lib-sdk-core/dist/browser/app/DefaultRootErrorBoundary.mjs
function DefaultRootErrorBoundary() {
	const error = useRouteError();
	useEffect(() => {
		reportClientError(error, { source: "render" });
	}, [error]);
	return /* @__PURE__ */ jsx(FullPageWrapper, { children: /* @__PURE__ */ jsxs("div", { children: ["There was an error rendering this page. Check the browser console for more information.", false] }) });
}
// node_modules/@wasp/lib-sdk-core/dist/browser/auth/events.mjs
var apiEventsEmitter = mitt();
// node_modules/@wasp/lib-sdk-core/dist/browser/storage.mjs
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
// node_modules/@wasp/lib-sdk-core/dist/browser/operations/queryClient.mjs
var MAX_QUERY_RETRIES = 3;
var RETRYABLE_CLIENT_ERROR_STATUS_CODES = /* @__PURE__ */ new Set([408, 429]);
var resolveQueryClientInitialized;
var queryClientInitialized = new Promise((resolve) => {
	resolveQueryClientInitialized = resolve;
});
function initializeQueryClient() {
	const queryClient = new QueryClient(withWaspQueryDefaults({}));
	resolveQueryClientInitialized(queryClient);
}
function withWaspQueryDefaults(config) {
	return {
		...config,
		defaultOptions: {
			...config.defaultOptions,
			queries: {
				retry: shouldRetryQuery,
				...config.defaultOptions?.queries
			}
		}
	};
}
function shouldRetryQuery(failureCount, error) {
	if (failureCount >= MAX_QUERY_RETRIES) return false;
	return !(error instanceof HttpError && error.status >= 400 && error.status < 500 && !RETRYABLE_CLIENT_ERROR_STATUS_CODES.has(error.status));
}
// node_modules/@wasp/lib-sdk-core/dist/browser/auth/session.mjs
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
// node_modules/@wasp/lib-sdk-core/dist/utils/ansiColors.mjs
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
// node_modules/@wasp/lib-sdk-core/dist/env/validation.mjs
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
// node_modules/@wasp/lib-sdk-core/dist/router/interpolatePath.mjs
function interpolatePath(path, params, search, hash) {
	const interpolatedPath = params ? interpolatePathParams(path, params) : path;
	const interpolatedSearch = search ? `?${new URLSearchParams(search).toString()}` : "";
	const interpolatedHash = hash ? `#${hash}` : "";
	return interpolatedPath + interpolatedSearch + interpolatedHash;
}
function interpolatePathParams(path, params) {
	function mapPathPart(part) {
		if (part === "*") return encodeSplatParam(params["*"]);
		if (part.startsWith(":")) return encodePathParam(params[extractParamNameFromPathPart(part)]);
		return part;
	}
	const interpolatedPath = path.split("/").map(mapPathPart).filter((part) => part !== "").join("/");
	return path.startsWith("/") ? `/${interpolatedPath}` : interpolatedPath;
}
function encodePathParam(value) {
	return encodeURIComponent(value ?? "");
}
function encodeSplatParam(value) {
	return String(value ?? "").split("/").map(encodeURIComponent).join("/");
}
function extractParamNameFromPathPart(paramString) {
	if (paramString.endsWith("?")) return paramString.slice(1, -1);
	return paramString.slice(1);
}
// node_modules/@wasp/lib-sdk-core/dist/utils/url.mjs
function stripTrailingSlash(url) {
	return url?.replace(/\/$/, "");
}
// .wasp/out/sdk/wasp/dist/client/env/schema.js
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
// .wasp/out/sdk/wasp/dist/client/app/components/WaspApp.jsx
function WaspApp({ children }) {
	const queryClient = use(queryClientInitialized);
	return /* @__PURE__ */ jsx(QueryClientProvider, {
		client: queryClient,
		children
	});
}
// .wasp/out/sdk/wasp/dist/client/router/index.js
var routes = { RootRoute: {
	to: "/",
	build: (options) => interpolatePath("/", void 0, options?.search, options?.hash)
} };
// .wasp/out/sdk/wasp/dist/client/app/router.jsx
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
// \0vite/preload-helper.js
var scriptRel = "modulepreload";
var assetsURL = function(dep) {
	return "/" + dep;
};
var seen = {};
var isCssPreloadUrl = function isCssPreloadUrl(url) {
	return url.pathname.endsWith(".css");
};
var preloadOnce = function preloadOnce(seen, href, preload) {
	if (href in seen) return seen[href];
	const promise = preload();
	if (!promise) {
		seen[href] = void 0;
		return;
	}
	const preloadPromise = promise.then(() => {
		seen[href] = void 0;
	}, (err) => {
		seen[href] = void 0;
		throw err;
	});
	seen[href] = preloadPromise;
	return preloadPromise;
};
var __vitePreload = function preload(baseModule, deps, importerUrl) {
	let promise = Promise.resolve();
	if (deps && deps.length > 0) {
		let preloadedHrefs;
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
			if (import.meta.resolve) return new URL(import.meta.resolve(specifier));
			return new URL(
				specifier,
				/** #__KEEP__ */
				import.meta.url
			);
		}
		promise = allSettled(deps.map((depString) => {
			depString = assetsURL(depString, importerUrl);
			const dep = importMetaResolve(depString);
			const isCss = isCssPreloadUrl(dep);
			return preloadOnce(seen, dep.href, () => {
				if (preloadedHrefs === void 0) {
					preloadedHrefs = {
						all: /* @__PURE__ */ new Set(),
						styles: /* @__PURE__ */ new Set()
					};
					const links = document.getElementsByTagName("link");
					for (let i = links.length - 1; i >= 0; i--) {
						const link = links[i];
						preloadedHrefs.all.add(link.href);
						if (link.rel === "stylesheet") preloadedHrefs.styles.add(link.href);
					}
				}
				if ((isCss ? preloadedHrefs.styles : preloadedHrefs.all).has(dep.href)) return;
				const link = document.createElement("link");
				link.rel = isCss ? "stylesheet" : scriptRel;
				if (!isCss) link.as = "script";
				link.crossOrigin = "";
				link.href = dep.href;
				if (cspNonce) link.setAttribute("nonce", cspNonce);
				document.head.appendChild(link);
				if (isCss) return new Promise((res, rej) => {
					const handleSettle = (e) => {
						link.removeEventListener("load", handleSettle);
						link.removeEventListener("error", handleSettle);
						if (e.type === "error") rej(/* @__PURE__ */ new Error(`Unable to preload CSS for ${dep}`));
						else res(e);
					};
					link.addEventListener("load", handleSettle);
					link.addEventListener("error", handleSettle);
				});
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
// routes.tsx
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
// client-entry.tsx
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
listenForUncaughtClientErrors();
startTransition(() => {
	hydrateRoot(document, fullAppTree, {
		onUncaughtError: reportRenderError,
		onCaughtError: reportRenderError
	});
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
