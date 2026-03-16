import fs from "node:fs";

export function getIndexTsxContent(): string {
  return fs.readFileSync(
    new URL("./files/index.tsx", import.meta.url),
    "utf-8"
  );
}

export function getRoutesTsxContent(): string {
  return fs.readFileSync(
    new URL("./files/routes.tsx", import.meta.url),
    "utf-8"
  );
}

export function getIndexHtmlContent(): string {
  return fs.readFileSync(
    new URL("./files/index.html", import.meta.url),
    "utf-8"
  );
}
