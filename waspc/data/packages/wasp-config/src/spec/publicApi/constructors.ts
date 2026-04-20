import type { Page, Query } from "./tsAppSpec.js";

export function page(
  component: Page["component"],
  config?: Pick<Page, "authRequired">,
): Page {
  return { part: "page", component, ...config };
}

export function query(
  fn: Query["fn"],
  config?: Pick<Query, "entities" | "auth">,
): Query {
  return { part: "query", fn, ...config };
}
