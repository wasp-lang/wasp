import type {
  Action,
  Api,
  ApiNamespace,
  App,
  Page,
  Query,
  Route,
} from "./tsAppSpec.js";

export function app(input: Omit<App, "kind">): App {
  return input;
}

export function page(
  component: Page["component"],
  config?: Pick<Page, "authRequired">,
): Page {
  return { kind: "page", component, ...config };
}

export function route(
  name: Route["name"],
  path: Route["path"],
  page: Route["page"],
  config?: Pick<Route, "lazy" | "prerender">,
): Route {
  return { kind: "route", name, path, page, ...config };
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

export function api(
  method: Api["method"],
  path: Api["path"],
  fn: Api["fn"],
  config?: Pick<Api, "middlewareConfigFn" | "entities" | "auth">,
): Api {
  return { kind: "api", method, path, fn, ...config };
}

export function apiNamespace(
  path: ApiNamespace["path"],
  config: Pick<ApiNamespace, "middlewareConfigFn">,
): ApiNamespace {
  return { kind: "apiNamespace", path, ...config };
}
