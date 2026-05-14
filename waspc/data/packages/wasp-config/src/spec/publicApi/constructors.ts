import type {
  Action,
  App,
  Auth,
  Job,
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

export function auth(config: Auth): Auth {
  return config;
}

export function job(
  fn: Job["fn"],
  config: Pick<
    Job,
    "executor" | "schedule" | "entities" | "performExecutorOptions"
  >,
): Job {
  return { kind: "job", fn, ...config };
}
