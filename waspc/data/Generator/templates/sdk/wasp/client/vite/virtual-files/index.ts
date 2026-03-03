import fs from "node:fs";

export function getIndexTsxContent(): string {
  return getFileContentFromRelativePath("./files/index.tsx");
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
