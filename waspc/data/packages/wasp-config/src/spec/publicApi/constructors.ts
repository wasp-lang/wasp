import type { Page, Query, TsAppSpec } from "./tsAppSpec.js";

export function app(input: Omit<TsAppSpec, "kind">): TsAppSpec {
  return { kind: "app", ...input };
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
