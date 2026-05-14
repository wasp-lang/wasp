/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */

import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import {
  action,
  api,
  apiNamespace,
  app,
  job,
  page,
  query,
  route,
} from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

export function getMinimalApp(): TsAppSpec.App {
  return app({
    name: "MinimalApp",
    wasp: { version: "^0.16.3" },
    title: "Mock App",
    parts: [],
  });
}

export function getMinimalAppWithParts(parts: TsAppSpec.Part[]): TsAppSpec.App {
  return {
    ...getMinimalApp(),
    parts,
  };
}

export function getPage<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Page>;
export function getPage(scope: ConfigScope): Config<TsAppSpec.Page> {
  switch (scope) {
    case "minimal":
      return page(getExtImport("minimal", "named"));
    case "full":
      return page(getExtImport("full", "named"), {
        authRequired: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getRoute<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Route>;
export function getRoute(scope: ConfigScope): Config<TsAppSpec.Route> {
  switch (scope) {
    case "minimal":
      return route("minimalRoute", "/foo/bar", getPage("minimal"));
    case "full":
      return route("fullRoute", "/foo/bar", getPage("full"), {
        lazy: true,
        prerender: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getQuery<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Query>;
export function getQuery(scope: ConfigScope): Config<TsAppSpec.Query> {
  switch (scope) {
    case "minimal":
      return query(getExtImport("minimal", "named"));
    case "full":
      return query(getExtImport("full", "named"), {
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getAction<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Action>;
export function getAction(scope: ConfigScope): Config<TsAppSpec.Action> {
  switch (scope) {
    case "minimal":
      return action(getExtImport("minimal", "named"));
    case "full":
      return action(getExtImport("full", "named"), {
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getApi<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Api>;
export function getApi(scope: ConfigScope): Config<TsAppSpec.Api> {
  switch (scope) {
    case "minimal":
      return api("GET", "/foo/bar", getExtImport("minimal", "named"));
    case "full":
      return api("POST", "/foo/bar", getExtImport("full", "named"), {
        middlewareConfigFn: getExtImport("full", "named"),
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getApiNamespace<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.ApiNamespace>;
export function getApiNamespace(
  scope: ConfigScope,
): Config<TsAppSpec.ApiNamespace> {
  switch (scope) {
    case "minimal":
      return apiNamespace("/foo", {
        middlewareConfigFn: getExtImport("minimal", "named"),
      });
    case "full":
      return apiNamespace("/foo", {
        middlewareConfigFn: getExtImport("full", "named"),
      });
    default:
      assertUnreachable(scope);
  }
}

export function getJob<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Job>;
export function getJob(scope: ConfigScope): Config<TsAppSpec.Job> {
  switch (scope) {
    case "minimal":
      return job(getExtImport("minimal", "named"), {
        executor: "PgBoss",
      });
    case "full":
      return job(getExtImport("full", "named"), {
        executor: "PgBoss",
        schedule: getSchedule("full"),
        entities: ["Task"],
        performExecutorOptions: { pgBoss: { jobOptions: { attempts: 3 } } },
      });
    default:
      assertUnreachable(scope);
  }
}

export function getSchedule<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Schedule>;
export function getSchedule(scope: ConfigScope): Config<TsAppSpec.Schedule> {
  switch (scope) {
    case "minimal":
      return { cron: "0 0 * * *" };
    case "full":
      return {
        cron: "0 0 * * *",
        args: { foo: "bar" },
        executorOptions: { pgBoss: { jobOptions: { attempts: 3 } } },
      };
    default:
      assertUnreachable(scope);
  }
}

export function getEntities(scope: ConfigScope): string[] {
  switch (scope) {
    case "minimal":
      return [];
    case "full":
      return ["Task"];
    default:
      assertUnreachable(scope);
  }
}

export function getExtImport<
  Scope extends ConfigScope,
  Kind extends AppSpec.ExtImportKind,
>(scope: Scope, importKind: Kind): ConfigFor<Scope, ExtImportFor<Kind>>;
export function getExtImport(
  scope: ConfigScope,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport> {
  switch (importKind) {
    case "named":
      return scope === "full"
        ? { import: "namedExport", alias: "namedAlias", from: "@src/external" }
        : { import: "namedExport", from: "@src/external" };
    case "default":
      return { importDefault: "defaultExport", from: "@src/external" };
    default:
      assertUnreachable(importKind);
  }
}

export type Config<T> = MinimalConfig<T> | FullConfig<T>;

/**
 * Recursively strips optional properties from T.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects keep only required keys (collapse to EmptyObject when none remain).
 * - Primitives pass through.
 */
export type MinimalConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer Item>
      ? Array<MinimalConfig<Item>>
      : T extends object
        ? keyof T extends never
          ? EmptyObject
          : MinimalConfigObject<T>
        : T;

type MinimalConfigObject<T> = {
  [K in keyof T as EmptyObject extends Pick<T, K> ? never : K]: MinimalConfig<
    T[K]
  >;
} extends infer Result
  ? Result extends EmptyObject
    ? EmptyObject
    : Result
  : never;

type EmptyObject = Record<string, never>;

/**
 * Recursively makes every property of T required.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects strip every `?` and recurse.
 * - Primitives pass through.
 */
export type FullConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer Item>
      ? Array<FullConfig<Item>>
      : T extends object
        ? FullConfigObject<T>
        : T;

type FullConfigObject<T> = {
  [K in keyof T]-?: FullConfig<T[K]>;
};

type ExtImportFor<Kind extends AppSpec.ExtImportKind> = Kind extends "named"
  ? TsAppSpec.NamedExtImport
  : TsAppSpec.DefaultExtImport;

type ConfigFor<Scope extends ConfigScope, Data> = Scope extends "full"
  ? FullConfig<Data>
  : MinimalConfig<Data>;

const CONFIG_SCOPES = ["minimal", "full"] as const;
type ConfigScope = (typeof CONFIG_SCOPES)[number];

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}
