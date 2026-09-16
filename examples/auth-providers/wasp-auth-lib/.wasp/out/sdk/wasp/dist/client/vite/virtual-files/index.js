import fs from "node:fs";
export function getClientEntryTsxContent() {
    return getFileContentFromRelativePath("./files/client-entry.tsx");
}
export function getRoutesTsxContent() {
    return getFileContentFromRelativePath("./files/routes.tsx");
}
export function getSsrEntryTsxContent() {
    return getFileContentFromRelativePath("./files/ssr-entry.tsx");
}
function getFileContentFromRelativePath(relativePath) {
    return fs.readFileSync(new URL(relativePath, import.meta.url), "utf-8");
}
//# sourceMappingURL=index.js.map