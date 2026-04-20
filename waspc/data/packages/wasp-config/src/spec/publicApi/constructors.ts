import type { Page, Query, Route } from "./tsAppSpec.js";

export function page(
  component: Page["component"],
  config?: Pick<Page, "authRequired">,
): Page {
  return { part: "page", component, ...config };
}

export function route(path: Route["path"], page: Route["page"]): Route {
  return { part: "route", path, page };
}

export function query(
  fn: Query["fn"],
  config?: Pick<Query, "entities" | "auth">,
): Query {
  return { part: "query", fn, ...config };
}
