/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */

import { pathToFileURL } from "node:url";
import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import {
  action,
  api,
  apiNamespace,
  app,
  job,
  makeRefImport,
  page,
  query,
  route,
} from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import type { AnyFunction } from "../../src/typeUtils.js";

export function getApp(scope: ConfigScope): TsAppSpec.App {
  switch (scope) {
    case "minimal":
      return app({
        name: "MinimalApp",
        wasp: { version: "^0.16.3" },
        title: "Mock App",
        decls: [],
      });
    case "full":
      return app({
        name: "FullApp",
        wasp: { version: "^0.16.3" },
        title: "Mock App",
        head: ['<link rel="icon" href="/favicon.ico" />'],
        auth: getAuthConfig("full"),
        server: getServerConfig("full"),
        client: getClientConfig("full"),
        db: getDbConfig("full"),
        emailSender: getEmailSenderConfig("full"),
        webSocket: getWebSocketConfig("full"),
        decls: [
          getPage("full"),
          getRoute("full"),
          getQuery("full"),
          getJob("full"),
          getCrud("full"),
          getEmailVerifyRoute(),
          getPasswordResetRoute(),
        ],
      });
    default:
      assertUnreachable(scope);
  }
}

export function getMinimalAppWithDecls(decls: TsAppSpec.Decl[]): TsAppSpec.App {
  return {
    ...getApp("minimal"),
    decls,
  };
}

export function getPage<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Page>;
export function getPage(scope: ConfigScope): Config<TsAppSpec.Page> {
  switch (scope) {
    case "minimal":
      return page(getRefImport("minimal", "named"));
    case "full":
      return page(getRefImport("full", "named"), {
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
      return query(getRefImport("minimal", "named"));
    case "full":
      return query(getRefImport("full", "named"), {
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
      return action(getRefImport("minimal", "named"));
    case "full":
      return action(getRefImport("full", "named"), {
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
      return api("GET", "/foo/bar", getRefImport("minimal", "named"));
    case "full":
      return api("POST", "/foo/bar", getRefImport("full", "named"), {
        middlewareConfigFn: getRefImport("full", "named"),
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
        middlewareConfigFn: getRefImport("minimal", "named"),
      });
    case "full":
      return apiNamespace("/foo", {
        middlewareConfigFn: getRefImport("full", "named"),
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
      return job(getRefImport("minimal", "named"), {
        executor: "PgBoss",
      });
    case "full":
      return job(getRefImport("full", "named"), {
        executor: "PgBoss",
        schedule: getSchedule("full"),
        entities: ["Task"],
        performExecutorOptions: { pgBoss: { jobOptions: { attempts: 3 } } },
      });
    default:
      assertUnreachable(scope);
  }
}

export function getCrud<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Crud>;
export function getCrud(scope: ConfigScope): Config<TsAppSpec.Crud> {
  // NOTE: Unlike the other decls, this fixture builds the object literally
  // instead of calling the `crud()` constructor. `crud()` returns the public
  // `Crud` type, whose `operations: CrudOperations` (all fields optional)
  // satisfies neither `MinimalConfig<Crud>` (operations collapse to an empty
  // object) nor `FullConfig<Crud>` (operations become fully required). Building
  // the literal lets us keep the public type narrow, which is simpler for
  // consumers.

  switch (scope) {
    case "minimal":
      return {
        kind: "crud",
        name: "minimalCrud",
        entity: "Task",
        operations: getCrudOperations("minimal"),
      } satisfies MinimalConfig<TsAppSpec.Crud>;
    case "full":
      return {
        kind: "crud",
        name: "fullCrud",
        entity: "Task",
        operations: getCrudOperations("full"),
      } satisfies FullConfig<TsAppSpec.Crud>;
    default:
      assertUnreachable(scope);
  }
}

export function getCrudOperations<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.CrudOperations>;
export function getCrudOperations(
  scope: ConfigScope,
): Config<TsAppSpec.CrudOperations> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.CrudOperations>;
    case "full":
      return {
        get: getCrudOperationOptions("full"),
        getAll: getCrudOperationOptions("full"),
        create: getCrudOperationOptions("full"),
        update: getCrudOperationOptions("full"),
        delete: getCrudOperationOptions("full"),
      } satisfies FullConfig<TsAppSpec.CrudOperations>;
    default:
      assertUnreachable(scope);
  }
}

export function getCrudOperationOptions<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: ConfigScope,
): Config<TsAppSpec.CrudOperationOptions> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.CrudOperationOptions>;
    case "full":
      return {
        isPublic: true,
        overrideFn: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.CrudOperationOptions>;
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
      return ["Task", "User", "SocialUser"];
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
        setupFn: getRefImport("full", "named"),
        middlewareConfigFn: getRefImport("full", "named"),
        envValidationSchema: getRefImport("full", "named"),
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
        rootComponent: getRefImport("full", "named"),
        setupFn: getRefImport("full", "named"),
        baseDir: "/src",
        envValidationSchema: getRefImport("full", "named"),
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
        seeds: [getRefImport("full", "named"), getRefImport("full", "default")],
        prismaSetupFn: getRefImport("full", "named"),
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

export function getWebSocketConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.WebSocket>;
export function getWebSocketConfig(
  scope: ConfigScope,
): Config<TsAppSpec.WebSocket> {
  switch (scope) {
    case "minimal":
      return {
        fn: getRefImport("minimal", "named"),
      } satisfies MinimalConfig<TsAppSpec.WebSocket>;
    case "full":
      return {
        fn: getRefImport("full", "named"),
        autoConnect: true,
      } satisfies FullConfig<TsAppSpec.WebSocket>;
    default:
      assertUnreachable(scope);
  }
}

export function getAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.Auth>;
export function getAuthConfig(scope: ConfigScope): Config<TsAppSpec.Auth> {
  switch (scope) {
    case "minimal":
      return {
        userEntity: "User",
        methods: getAuthMethods("minimal"),
        onAuthFailedRedirectTo: "/login",
      } satisfies MinimalConfig<TsAppSpec.Auth>;
    case "full":
      return {
        userEntity: "User",
        methods: getAuthMethods("full"),
        onAuthFailedRedirectTo: "/login",
        onAuthSucceededRedirectTo: "/profile",
        onBeforeSignup: getRefImport("full", "named"),
        onAfterSignup: getRefImport("full", "named"),
        onAfterEmailVerified: getRefImport("full", "named"),
        onBeforeOAuthRedirect: getRefImport("full", "named"),
        onBeforeLogin: getRefImport("full", "named"),
        onAfterLogin: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.Auth>;
    default:
      assertUnreachable(scope);
  }
}

export function getAuthMethods<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.AuthMethods>;
export function getAuthMethods(
  scope: ConfigScope,
): Config<TsAppSpec.AuthMethods> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.AuthMethods>;
    case "full":
      return {
        slack: getSocialAuthConfig("full"),
        discord: getSocialAuthConfig("full"),
        google: getSocialAuthConfig("full"),
        gitHub: getSocialAuthConfig("full"),
        keycloak: getSocialAuthConfig("full"),
        microsoft: getSocialAuthConfig("full"),
        email: getEmailAuthConfig("full"),
      } satisfies FullConfig<TsAppSpec.AuthMethods>;
    default:
      assertUnreachable(scope);
  }
}

export function getUsernameAndPasswordConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.UsernameAndPasswordConfig>;
export function getUsernameAndPasswordConfig(
  scope: ConfigScope,
): Config<TsAppSpec.UsernameAndPasswordConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.UsernameAndPasswordConfig>;
    case "full":
      return {
        userSignupFields: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.UsernameAndPasswordConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getSocialAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.SocialAuthConfig>;
export function getSocialAuthConfig(
  scope: ConfigScope,
): Config<TsAppSpec.SocialAuthConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.SocialAuthConfig>;
    case "full":
      return {
        configFn: getRefImport("full", "named"),
        userSignupFields: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.SocialAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.EmailAuthConfig>;
export function getEmailAuthConfig(
  scope: ConfigScope,
): Config<TsAppSpec.EmailAuthConfig> {
  switch (scope) {
    case "minimal":
      return {
        fromField: getEmailFromField("minimal"),
        emailVerification: getEmailVerificationConfig("minimal"),
        passwordReset: getPasswordResetConfig("minimal"),
      } satisfies MinimalConfig<TsAppSpec.EmailAuthConfig>;
    case "full":
      return {
        fromField: getEmailFromField("full"),
        emailVerification: getEmailVerificationConfig("full"),
        passwordReset: getPasswordResetConfig("full"),
        userSignupFields: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.EmailAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailVerificationConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.EmailFlowConfig>;
export function getEmailVerificationConfig(
  scope: ConfigScope,
): Config<TsAppSpec.EmailFlowConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: EMAIL_VERIFY_ROUTE_NAME,
      } satisfies MinimalConfig<TsAppSpec.EmailFlowConfig>;
    case "full":
      return {
        clientRoute: EMAIL_VERIFY_ROUTE_NAME,
        getEmailContentFn: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.EmailFlowConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getPasswordResetConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, TsAppSpec.EmailFlowConfig>;
export function getPasswordResetConfig(
  scope: ConfigScope,
): Config<TsAppSpec.EmailFlowConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: PASSWORD_RESET_ROUTE_NAME,
      } satisfies MinimalConfig<TsAppSpec.EmailFlowConfig>;
    case "full":
      return {
        clientRoute: PASSWORD_RESET_ROUTE_NAME,
        getEmailContentFn: getRefImport("full", "named"),
      } satisfies FullConfig<TsAppSpec.EmailFlowConfig>;
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

export const EMAIL_VERIFY_ROUTE_PATH = "/email-verify";
export const EMAIL_VERIFY_ROUTE_NAME = "EmailVerifyRoute";
export const PASSWORD_RESET_ROUTE_PATH = "/password-reset";
export const PASSWORD_RESET_ROUTE_NAME = "PasswordResetRoute";

export function getEmailVerifyRoute(): TsAppSpec.Route {
  return route(
    EMAIL_VERIFY_ROUTE_NAME,
    EMAIL_VERIFY_ROUTE_PATH,
    page(
      getRefImportForMockProject({
        import: "EmailVerifyPage",
        from: "./src/auth/pages",
      }),
    ),
  );
}

export function getPasswordResetRoute(): TsAppSpec.Route {
  return route(
    PASSWORD_RESET_ROUTE_NAME,
    PASSWORD_RESET_ROUTE_PATH,
    page(
      getRefImportForMockProject({
        import: "PasswordResetPage",
        from: "./src/auth/pages",
      }),
    ),
  );
}

export function getRefImport<
  Scope extends ConfigScope,
  Kind extends AppSpec.ExtImportKind,
>(scope: Scope, importKind: Kind): ConfigFor<Scope, RefImportFor<Kind>>;
export function getRefImport(
  scope: ConfigScope,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.RefImport> {
  switch (importKind) {
    case "named":
      return scope === "full"
        ? getRefImportForMockProject({
            import: "namedExport",
            alias: "namedAlias",
            from: "./src/external",
          })
        : getRefImportForMockProject({
            import: "namedExport",
            from: "./src/external",
          });
    case "default":
      return getRefImportForMockProject({
        importDefault: "defaultExport",
        from: "./src/external",
      });
    default:
      assertUnreachable(importKind);
  }
}

const getRefImportForMockProject = makeRefImport(
  pathToFileURL("/project/main.wasp.ts").href,
);

export type Config<T> = MinimalConfig<T> | FullConfig<T>;

/**
 * Recursively strips optional properties from T.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects keep only required keys (collapse to EmptyObject when none remain).
 * - Primitives pass through.
 * - Functions pass through.
 */
export type MinimalConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends AnyFunction
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
 * - Functions pass through.
 *
 * Exception: keys that can only ever be `never` (the exclusion markers
 * generated by `RequireOneOrNone` & friends, e.g. `usernameAndPassword?: never`
 * on the email branch of `AuthMethods`) are left optional. Forcing them to be
 * required would make the type unsatisfiable.
 */
export type FullConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends AnyFunction
      ? T
      : T extends Array<infer Item>
        ? Array<FullConfig<Item>>
        : T extends object
          ? FullConfigObject<T>
          : T;

type FullConfigObject<T> = {
  [K in keyof T as IsExclusionMarker<T[K]> extends true
    ? never
    : K]-?: FullConfig<T[K]>;
} & {
  [K in keyof T as IsExclusionMarker<T[K]> extends true ? K : never]?: T[K];
};

// True when a property's only non-`undefined` value is `never`, i.e. the
// property is a "this key must be absent" exclusion marker.
type IsExclusionMarker<V> = [Exclude<V, undefined>] extends [never]
  ? true
  : false;

type RefImportFor<Kind extends AppSpec.ExtImportKind> = Kind extends "named"
  ? TsAppSpec.RefImport<TsAppSpec.NamedRefImportDescriptor>
  : TsAppSpec.RefImport<TsAppSpec.DefaultRefImportDescriptor>;

type ConfigFor<Scope extends ConfigScope, Data> = Scope extends "full"
  ? FullConfig<Data>
  : MinimalConfig<Data>;

const CONFIG_SCOPES = ["minimal", "full"] as const;
type ConfigScope = (typeof CONFIG_SCOPES)[number];

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}
