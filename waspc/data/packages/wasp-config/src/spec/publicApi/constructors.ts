import type {
  Action,
  Api,
  ApiNamespace,
  App,
  Job,
  Page,
  Part,
  Query,
  Route,
} from "./tsAppSpec.js";

export function app<const Parts extends readonly Part<string>[]>(
  input: Omit<App<Parts>, "kind">,
): App<Parts> {
  return input;
}

export function page(
  component: Page["component"],
  config?: Pick<Page, "authRequired">,
): Page {
  return { kind: "page", component, ...config };
}

export function route<const Path extends string>(
  name: Route<Path>["name"],
  path: Route<Path>["path"],
  page: Route<Path>["page"],
  config?: Pick<Route<Path>, "lazy" | "prerender">,
): Route<Path> {
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

export function api<const Path extends string>(
  method: Api<Path>["method"],
  path: Api<Path>["path"],
  fn: Api<Path>["fn"],
  config?: Pick<Api<Path>, "middlewareConfigFn" | "entities" | "auth">,
): Api<Path> {
  return { kind: "api", method, path, fn, ...config };
}

export function apiNamespace(
  path: ApiNamespace["path"],
  config: Pick<ApiNamespace, "middlewareConfigFn">,
): ApiNamespace {
  return { kind: "apiNamespace", path, ...config };
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
