//#region src/index.ts
function resolveWaspApiPath(config, routePath) {
	return `${config.waspApiMountPath.replace(/\/$/, "")}${routePath.startsWith("/") ? routePath : `/${routePath}`}`;
}
function resolveWaspApiUrl(config, serverUrl, routePath) {
	return `${serverUrl.replace(/\/$/, "")}${resolveWaspApiPath(config, routePath)}`;
}
//#endregion
export { resolveWaspApiPath, resolveWaspApiUrl };

//# sourceMappingURL=index.js.map