import type { App, Page, Query } from "./tsAppSpec.js";

export function app(input: Omit<App, "kind">): App {
  return input;
}

export function page(
  component: Page["component"],
  config?: Pick<Page, "authRequired">,
): Page {
  return { kind: "page", component, ...config };
}

export function query(
  fn: Query["fn"],
  config?: Pick<Query, "entities" | "auth">,
): Query {
  return { kind: "query", fn, ...config };
}
