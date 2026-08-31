//#region src/index.ts
function resolveWaspApiPath(config, routePath) {
	return `${config.waspApiMountPath.replace(/\/$/, "")}${routePath.startsWith("/") ? routePath : `/${routePath}`}`;
}
function resolveWaspApiUrl(config, serverUrl, routePath) {
	return `${serverUrl.replace(/\/$/, "")}${resolveWaspApiPath(config, routePath)}`;
}
//#endregion
//#region src/browser.ts
const sessionStorageKey = "sessionId";
function configureBrowserAppDelivery(options) {
	const { config, storage } = options;
	const serverUrl = config.mode === "integrated" && typeof window !== "undefined" ? window.location.origin : config.serverUrl;
	const currentSessionId = () => {
		if (config.mode === "integrated") return null;
		const sessionId = storage.get(sessionStorageKey);
		return typeof sessionId === "string" ? sessionId : null;
	};
	return {
		serverUrl,
		waspApiPath: (routePath) => resolveWaspApiPath(config, routePath),
		waspApiUrl: (routePath) => resolveWaspApiUrl(config, serverUrl, routePath),
		prepareHttpRequest: (request) => {
			if (config.mode === "integrated") {
				const csrfToken = readBrowserCookie("wasp_csrf");
				if (csrfToken) request.headers.set("X-Wasp-CSRF", csrfToken);
				return;
			}
			const sessionId = currentSessionId();
			if (sessionId) request.headers.set("Authorization", `Bearer ${sessionId}`);
		},
		acceptSession: (sessionId) => {
			if (config.mode === "split") storage.set(sessionStorageKey, sessionId);
		},
		clearSession: () => {
			if (config.mode === "split") storage.remove(sessionStorageKey);
		},
		clearLocalData: () => {
			if (config.mode === "split") storage.clear();
		},
		currentSessionId,
		sessionIdFromAuthorizationHeader: (header) => {
			return header?.startsWith("Bearer ") ? header.slice(7) : null;
		},
		socketConnectionOptions: () => ({
			withCredentials: config.mode === "integrated",
			auth: { sessionId: currentSessionId() }
		})
	};
}
function readBrowserCookie(name) {
	if (typeof document === "undefined") return null;
	const prefix = `${name}=`;
	const value = document.cookie.split(";").map((part) => part.trim()).find((part) => part.startsWith(prefix));
	return value ? decodeURIComponent(value.slice(prefix.length)) : null;
}
//#endregion
export { configureBrowserAppDelivery };

//# sourceMappingURL=browser.js.map