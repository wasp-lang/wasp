import fs from "node:fs";

export function getClientEntryTsxContent(): string {
  return getFileContentFromRelativePath("./files/client-entry.tsx");
}

export function getClientAppTsxContent(): string {
  return getFileContentFromRelativePath("./files/client-app.tsx");
}

export function getRoutesTsxContent(): string {
  return getFileContentFromRelativePath("./files/routes.tsx");
}

export function getSsrEntryTsxContent(): string {
  return getFileContentFromRelativePath("./files/ssr-entry.tsx");
}

function getFileContentFromRelativePath(relativePath: string): string {
  return fs.readFileSync(
    new URL(relativePath, import.meta.url),
    "utf-8"
  );
}
