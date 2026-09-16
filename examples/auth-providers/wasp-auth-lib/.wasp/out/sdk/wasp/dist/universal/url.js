export function stripTrailingSlash(url) {
    return url?.replace(/\/$/, "");
}
export function getOrigin(url) {
    return new URL(url).origin;
}
//# sourceMappingURL=url.js.map