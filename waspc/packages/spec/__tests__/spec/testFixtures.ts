/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */

import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import { _waspMakeRef } from "../../src/internal.js";
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
import * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";
import type { AnyFunction } from "../../src/typeUtils.js";

export const MOCK_PROJECT_DIR = "/project";
export const MOCK_MAIN_WASP_TS_PATH = `${MOCK_PROJECT_DIR}/main.wasp.ts`;

export function getApp(scope: ConfigScope): WaspSpec.App {
  switch (scope) {
    case "minimal":
      return app({
        name: "MinimalApp",
        wasp: { version: "^0.16.3" },
        title: "Mock App",
        spec: [],
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
        spec: [
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

export function getMinimalAppWithSpec(spec: WaspSpec.Spec): WaspSpec.App {
  return {
    ...getApp("minimal"),
    spec,
  };
}

export function getPage<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Page>;
export function getPage(scope: ConfigScope): Config<WaspSpec.Page> {
  switch (scope) {
    case "minimal":
      return page(getRefObject("minimal", "named"));
    case "full":
      return page(getRefObject("full", "named"), {
        authRequired: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getRoute<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Route>;
export function getRoute(scope: ConfigScope): Config<WaspSpec.Route> {
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
): ConfigFor<Scope, WaspSpec.Query>;
export function getQuery(scope: ConfigScope): Config<WaspSpec.Query> {
  switch (scope) {
    case "minimal":
      return query(getRefObject("minimal", "named"));
    case "full":
      return query(getRefObject("full", "named"), {
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getAction<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Action>;
export function getAction(scope: ConfigScope): Config<WaspSpec.Action> {
  switch (scope) {
    case "minimal":
      return action(getRefObject("minimal", "named"));
    case "full":
      return action(getRefObject("full", "named"), {
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getApi<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Api>;
export function getApi(scope: ConfigScope): Config<WaspSpec.Api> {
  switch (scope) {
    case "minimal":
      return api("GET", "/foo/bar", getRefObject("minimal", "named"));
    case "full":
      return api("POST", "/foo/bar", getRefObject("full", "named"), {
        middlewareConfigFn: getRefObject("full", "named"),
        entities: ["Task"],
        auth: true,
      });
    default:
      assertUnreachable(scope);
  }
}

export function getApiNamespace<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.ApiNamespace>;
export function getApiNamespace(
  scope: ConfigScope,
): Config<WaspSpec.ApiNamespace> {
  switch (scope) {
    case "minimal":
      return apiNamespace("/foo", {
        middlewareConfigFn: getRefObject("minimal", "named"),
      });
    case "full":
      return apiNamespace("/foo", {
        middlewareConfigFn: getRefObject("full", "named"),
      });
    default:
      assertUnreachable(scope);
  }
}

export function getJob<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Job>;
export function getJob(scope: ConfigScope): Config<WaspSpec.Job> {
  switch (scope) {
    case "minimal":
      return job(getRefObject("minimal", "named"), {
        executor: "PgBoss",
      });
    case "full":
      return job(getRefObject("full", "named"), {
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
): ConfigFor<Scope, WaspSpec.Crud>;
export function getCrud(scope: ConfigScope): Config<WaspSpec.Crud> {
  // NOTE: Unlike the other Spec Elements, this fixture builds the object literally
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
      } satisfies MinimalConfig<WaspSpec.Crud>;
    case "full":
      return {
        kind: "crud",
        name: "fullCrud",
        entity: "Task",
        operations: getCrudOperations("full"),
      } satisfies FullConfig<WaspSpec.Crud>;
    default:
      assertUnreachable(scope);
  }
}

export function getCrudOperations<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.CrudOperations>;
export function getCrudOperations(
  scope: ConfigScope,
): Config<WaspSpec.CrudOperations> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.CrudOperations>;
    case "full":
      return {
        get: getCrudOperationOptions("full"),
        getAll: getCrudOperationOptions("full"),
        create: getCrudOperationOptions("full"),
        update: getCrudOperationOptions("full"),
        delete: getCrudOperationOptions("full"),
      } satisfies FullConfig<WaspSpec.CrudOperations>;
    default:
      assertUnreachable(scope);
  }
}

export function getCrudOperationOptions<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: ConfigScope,
): Config<WaspSpec.CrudOperationOptions> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.CrudOperationOptions>;
    case "full":
      return {
        isPublic: true,
        overrideFn: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.CrudOperationOptions>;
    default:
      assertUnreachable(scope);
  }
}

export function getSchedule<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Schedule>;
export function getSchedule(scope: ConfigScope): Config<WaspSpec.Schedule> {
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
): ConfigFor<Scope, WaspSpec.Server>;
export function getServerConfig(scope: ConfigScope): Config<WaspSpec.Server> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.Server>;
    case "full":
      return {
        setupFn: getRefObject("full", "named"),
        middlewareConfigFn: getRefObject("full", "named"),
        envValidationSchema: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.Server>;
    default:
      assertUnreachable(scope);
  }
}

export function getClientConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Client>;
export function getClientConfig(scope: ConfigScope): Config<WaspSpec.Client> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.Client>;
    case "full":
      return {
        rootComponent: getRefObject("full", "named"),
        setupFn: getRefObject("full", "named"),
        baseDir: "/src",
        envValidationSchema: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.Client>;
    default:
      assertUnreachable(scope);
  }
}

export function getDbConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Db>;
export function getDbConfig(scope: ConfigScope): Config<WaspSpec.Db> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.Db>;
    case "full":
      return {
        seeds: [getRefObject("full", "named"), getRefObject("full", "default")],
        prismaSetupFn: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.Db>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailSenderConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.EmailSender>;
export function getEmailSenderConfig(
  scope: ConfigScope,
): Config<WaspSpec.EmailSender> {
  switch (scope) {
    case "minimal":
      return {
        provider: "SMTP",
      } satisfies MinimalConfig<WaspSpec.EmailSender>;
    case "full":
      return {
        provider: "SMTP",
        defaultFrom: getEmailFromField("full"),
      } satisfies FullConfig<WaspSpec.EmailSender>;
    default:
      assertUnreachable(scope);
  }
}

export function getWebSocketConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.WebSocket>;
export function getWebSocketConfig(
  scope: ConfigScope,
): Config<WaspSpec.WebSocket> {
  switch (scope) {
    case "minimal":
      return {
        fn: getRefObject("minimal", "named"),
      } satisfies MinimalConfig<WaspSpec.WebSocket>;
    case "full":
      return {
        fn: getRefObject("full", "named"),
        autoConnect: true,
      } satisfies FullConfig<WaspSpec.WebSocket>;
    default:
      assertUnreachable(scope);
  }
}

export function getAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.Auth>;
export function getAuthConfig(scope: ConfigScope): Config<WaspSpec.Auth> {
  switch (scope) {
    case "minimal":
      return {
        userEntity: "User",
        methods: getAuthMethods("minimal"),
        onAuthFailedRedirectTo: "/login",
      } satisfies MinimalConfig<WaspSpec.Auth>;
    case "full":
      return {
        userEntity: "User",
        methods: getAuthMethods("full"),
        onAuthFailedRedirectTo: "/login",
        onAuthSucceededRedirectTo: "/profile",
        onBeforeSignup: getRefObject("full", "named"),
        onAfterSignup: getRefObject("full", "named"),
        onAfterEmailVerified: getRefObject("full", "named"),
        onBeforeOAuthRedirect: getRefObject("full", "named"),
        onBeforeLogin: getRefObject("full", "named"),
        onAfterLogin: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.Auth>;
    default:
      assertUnreachable(scope);
  }
}

export function getAuthMethods<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.AuthMethods>;
export function getAuthMethods(
  scope: ConfigScope,
): Config<WaspSpec.AuthMethods> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.AuthMethods>;
    case "full":
      return {
        slack: getSocialAuthConfig("full"),
        discord: getSocialAuthConfig("full"),
        google: getSocialAuthConfig("full"),
        gitHub: getSocialAuthConfig("full"),
        keycloak: getSocialAuthConfig("full"),
        microsoft: getSocialAuthConfig("full"),
        email: getEmailAuthConfig("full"),
      } satisfies FullConfig<WaspSpec.AuthMethods>;
    default:
      assertUnreachable(scope);
  }
}

export function getUsernameAndPasswordConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.UsernameAndPasswordConfig>;
export function getUsernameAndPasswordConfig(
  scope: ConfigScope,
): Config<WaspSpec.UsernameAndPasswordConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.UsernameAndPasswordConfig>;
    case "full":
      return {
        userSignupFields: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.UsernameAndPasswordConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getSocialAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.SocialAuthConfig>;
export function getSocialAuthConfig(
  scope: ConfigScope,
): Config<WaspSpec.SocialAuthConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<WaspSpec.SocialAuthConfig>;
    case "full":
      return {
        configFn: getRefObject("full", "named"),
        userSignupFields: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.SocialAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailAuthConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.EmailAuthConfig>;
export function getEmailAuthConfig(
  scope: ConfigScope,
): Config<WaspSpec.EmailAuthConfig> {
  switch (scope) {
    case "minimal":
      return {
        fromField: getEmailFromField("minimal"),
        emailVerification: getEmailVerificationConfig("minimal"),
        passwordReset: getPasswordResetConfig("minimal"),
      } satisfies MinimalConfig<WaspSpec.EmailAuthConfig>;
    case "full":
      return {
        fromField: getEmailFromField("full"),
        emailVerification: getEmailVerificationConfig("full"),
        passwordReset: getPasswordResetConfig("full"),
        userSignupFields: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.EmailAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailVerificationConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.EmailFlowConfig>;
export function getEmailVerificationConfig(
  scope: ConfigScope,
): Config<WaspSpec.EmailFlowConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: EMAIL_VERIFY_ROUTE_NAME,
      } satisfies MinimalConfig<WaspSpec.EmailFlowConfig>;
    case "full":
      return {
        clientRoute: EMAIL_VERIFY_ROUTE_NAME,
        getEmailContentFn: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.EmailFlowConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getPasswordResetConfig<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.EmailFlowConfig>;
export function getPasswordResetConfig(
  scope: ConfigScope,
): Config<WaspSpec.EmailFlowConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: PASSWORD_RESET_ROUTE_NAME,
      } satisfies MinimalConfig<WaspSpec.EmailFlowConfig>;
    case "full":
      return {
        clientRoute: PASSWORD_RESET_ROUTE_NAME,
        getEmailContentFn: getRefObject("full", "named"),
      } satisfies FullConfig<WaspSpec.EmailFlowConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailFromField<Scope extends ConfigScope>(
  scope: Scope,
): ConfigFor<Scope, WaspSpec.EmailFromField>;
export function getEmailFromField(
  scope: ConfigScope,
): Config<WaspSpec.EmailFromField> {
  switch (scope) {
    case "minimal":
      return {
        email: "noreply@example.com",
      } satisfies MinimalConfig<WaspSpec.EmailFromField>;
    case "full":
      return {
        name: "Wasp",
        email: "noreply@example.com",
      } satisfies FullConfig<WaspSpec.EmailFromField>;
    default:
      assertUnreachable(scope);
  }
}

export const EMAIL_VERIFY_ROUTE_PATH = "/email-verify";
export const EMAIL_VERIFY_ROUTE_NAME = "EmailVerifyRoute";
export const PASSWORD_RESET_ROUTE_PATH = "/password-reset";
export const PASSWORD_RESET_ROUTE_NAME = "PasswordResetRoute";

export function getEmailVerifyRoute(): WaspSpec.Route {
  return route(
    EMAIL_VERIFY_ROUTE_NAME,
    EMAIL_VERIFY_ROUTE_PATH,
    page(
      getRefObjectForMockProject({
        import: "EmailVerifyPage",
        from: "./src/auth/pages",
      }),
    ),
  );
}

export function getPasswordResetRoute(): WaspSpec.Route {
  return route(
    PASSWORD_RESET_ROUTE_NAME,
    PASSWORD_RESET_ROUTE_PATH,
    page(
      getRefObjectForMockProject({
        import: "PasswordResetPage",
        from: "./src/auth/pages",
      }),
    ),
  );
}

export function getRefObject<
  Scope extends ConfigScope,
  Kind extends AppSpec.ExtImportKind,
>(scope: Scope, importKind: Kind): ConfigFor<Scope, RefObjectFor<Kind>>;
export function getRefObject(
  scope: ConfigScope,
  importKind: AppSpec.ExtImportKind,
): Config<WaspSpec.RefObject> {
  switch (importKind) {
    case "named":
      return scope === "full"
        ? getRefObjectForMockProject({
            import: "namedExport",
            alias: "namedAlias",
            from: "./src/external",
          })
        : getRefObjectForMockProject({
            import: "namedExport",
            from: "./src/external",
          });
    case "default":
      return getRefObjectForMockProject({
        importDefault: "defaultExport",
        from: "./src/external",
      });
    default:
      assertUnreachable(importKind);
  }
}

const getRefObjectForMockProject = _waspMakeRef(MOCK_MAIN_WASP_TS_PATH);

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

type RefObjectFor<Kind extends AppSpec.ExtImportKind> = Kind extends "named"
  ? WaspSpec.RefObject & WaspSpec.NamedRefObjectDescriptor
  : WaspSpec.RefObject & WaspSpec.DefaultRefObjectDescriptor;

type ConfigFor<Scope extends ConfigScope, Data> = Scope extends "full"
  ? FullConfig<Data>
  : MinimalConfig<Data>;

const CONFIG_SCOPES = ["minimal", "full"] as const;
type ConfigScope = (typeof CONFIG_SCOPES)[number];

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}
