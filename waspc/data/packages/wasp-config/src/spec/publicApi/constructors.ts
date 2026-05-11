import type { Action, App, Page, Query } from "./tsAppSpec.js";

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

export function action(
  fn: Action["fn"],
  config?: Pick<Action, "entities" | "auth">,
): Action {
  return { kind: "action", fn, ...config };
}
