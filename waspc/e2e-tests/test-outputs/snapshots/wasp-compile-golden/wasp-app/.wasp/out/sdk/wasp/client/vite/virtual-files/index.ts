import fs from "node:fs";
import { fileURLToPath } from "node:url";

const clientRuntimeBindingsFileUrl = new URL(
  "./files/client-runtime-bindings.ts",
  import.meta.url,
);

export const clientRuntimeBindingsFilePath = fileURLToPath(
  clientRuntimeBindingsFileUrl,
);

export function getClientEntryTsxContent(): string {
  return getFileContentFromRelativePath("./files/client-entry.tsx");
}

export function getClientRuntimeBindingsTsContent(): string {
  return fs.readFileSync(clientRuntimeBindingsFileUrl, "utf-8");
}

export function getRoutesTsxContent(): string {
  return getFileContentFromRelativePath("./files/routes.tsx");
}

export function getSsrEntryTsxContent(): string {
  return getFileContentFromRelativePath("./files/ssr-entry.tsx");
}

function getFileContentFromRelativePath(relativePath: string): string {
  return fs.readFileSync(new URL(relativePath, import.meta.url), "utf-8");
}
