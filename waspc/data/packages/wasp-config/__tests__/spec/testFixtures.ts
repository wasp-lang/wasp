/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */

import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import {
  action,
  app,
  job,
  page,
  query,
  route,
} from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

export function getApp(scope: ConfigScope): TsAppSpec.App {
  switch (scope) {
    case "minimal":
      return app({
        name: "MinimalApp",
        wasp: { version: "^0.16.3" },
        title: "Mock App",
        parts: [],
      });
    case "full":
      return app({
        name: "FullApp",
        wasp: { version: "^0.16.3" },
        title: "Mock App",
        head: ['<link rel="icon" href="/favicon.ico" />'],
        server: getServerConfig("full"),
        client: getClientConfig("full"),
        db: getDbConfig("full"),
        emailSender: getEmailSenderConfig("full"),
        webSocket: getWebSocketConfig("full"),
        parts: [
          getPage("full"),
          getRoute("full"),
          getQuery("full"),
          getJob("full"),
        ],
      });
    default:
      assertUnreachable(scope);
  }
}

export function getMinimalAppWithParts(parts: TsAppSpec.Part[]): TsAppSpec.App {
  return {
    ...getApp("minimal"),
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

export function getServerConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Server>;
export function getServerConfig(scope: ConfigScope): Config<TsAppSpec.Server> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.Server>;
    case "full":
      return {
        setupFn: getExtImport("full", "named"),
        middlewareConfigFn: getExtImport("full", "named"),
        envValidationSchema: getExtImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.Server>;
    default:
      assertUnreachable(scope);
  }
}

export function getClientConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Client>;
export function getClientConfig(scope: ConfigScope): Config<TsAppSpec.Client> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.Client>;
    case "full":
      return {
        rootComponent: getExtImport("full", "named"),
        setupFn: getExtImport("full", "named"),
        baseDir: "/src",
        envValidationSchema: getExtImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.Client>;
    default:
      assertUnreachable(scope);
  }
}

export function getDbConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Db>;
export function getDbConfig(scope: ConfigScope): Config<TsAppSpec.Db> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.Db>;
    case "full":
      return {
        seeds: [getExtImport("full", "named"), getExtImport("full", "default")],
        prismaSetupFn: getExtImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.Db>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailSenderConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.EmailSender>;
export function getEmailSenderConfig(
  scope: ConfigScope,
): Config<TsAppSpec.EmailSender> {
  switch (scope) {
    case "minimal":
      return {
        provider: "SMTP",
      } satisfies MinimalConfig<TsAppSpec.EmailSender>;
    case "full":
      return {
        provider: "SMTP",
        defaultFrom: getEmailFromField("full"),
      } satisfies FullConfig<TsAppSpec.EmailSender>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailFromField<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.EmailFromField>;
export function getEmailFromField(
  scope: ConfigScope,
): Config<TsAppSpec.EmailFromField> {
  switch (scope) {
    case "minimal":
      return {
        email: "noreply@example.com",
      } satisfies MinimalConfig<TsAppSpec.EmailFromField>;
    case "full":
      return {
        name: "Wasp",
        email: "noreply@example.com",
      } satisfies FullConfig<TsAppSpec.EmailFromField>;
    default:
      assertUnreachable(scope);
  }
}

export function getWebSocketConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.WebSocket>;
export function getWebSocketConfig(
  scope: ConfigScope,
): Config<TsAppSpec.WebSocket> {
  switch (scope) {
    case "minimal":
      return {
        fn: getExtImport("minimal", "named"),
      } satisfies MinimalConfig<TsAppSpec.WebSocket>;
    case "full":
      return {
        fn: getExtImport("full", "named"),
        autoConnect: true,
      } satisfies FullConfig<TsAppSpec.WebSocket>;
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
