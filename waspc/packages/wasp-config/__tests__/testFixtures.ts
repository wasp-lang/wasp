/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents TsAppSpec data.
 */

import * as AppSpec from "../src/appSpec.js";
import { Branded } from "../src/branded.js";
import { App } from "../src/publicApi/App.js";
import * as TsAppSpec from "../src/publicApi/tsAppSpec.js";

/**
 * Creates a type containing only the required properties from T recursively.
 *
 * This utility:
 * - Filters out optional properties recurisvely
 * - Stops from unwrapping `Branded` types
 * - Returns `EmptyObject` when no required properties exist
 *
 * @template T - The type to extract required properties from
 *
 * @example
 * ```ts
 * // Given the following type:
 * type Result = MinimalConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d?: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h?: string;
 *   }[];
 * };
 * // The result will be:
 * type Result = {
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *   };
 *   f: {
 *     g: boolean;
 *     h: string;
 *   }[];
 * };
 */
type MinimalConfig<T> =
  T extends Branded<infer TType, infer TBrand>
    ? Branded<TType, TBrand>
    : T extends Array<infer TArrayItem>
      ? Array<MinimalConfig<TArrayItem>>
      : T extends object
        ? keyof T extends never
          ? EmptyObject
          : {
              [K in keyof T as T[K] extends Required<T>[K]
                ? K
                : never]: MinimalConfig<T[K]>;
            }
        : T;

/**
 * Represents an empty object type in TypeScript.
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
type EmptyObject = Record<string, never>;

/**
 * Creates a type with all properties of T required recursively.
 *
 * This utility:
 * - Makes all properties required recursively
 * - Stops from unwrapping branded types
 *
 * @template T - The type to make fully required
 *
 * @example
 * ```ts
 * // Given the following type:
 * type Result = FullConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d?: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h?: string;
 *   }[];
 * };
 * // The result will be:
 * type Result = FullConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h: string;
 *   }[];
 * };
 */
type FullConfig<T> =
  T extends Branded<infer TType, infer TBrand>
    ? Branded<TType, TBrand>
    : T extends Array<infer TArrayItem>
      ? Array<FullConfig<TArrayItem>>
      : T extends object
        ? {
            [K in keyof T]-?: FullConfig<T[K]>;
          }
        : T;

export type Config<T> = MinimalConfig<T> | FullConfig<T>;

type MinimalNamedConfig<T> = {
  name: string;
  config: MinimalConfig<T>;
};

type FullNamedConfig<T> = {
  name: string;
  config: FullConfig<T>;
};

type NamedConfig<T> = MinimalNamedConfig<T> | FullNamedConfig<T>;

const CONFIG_TYPES = ["minimal", "full"] as const;
type ConfigType = (typeof CONFIG_TYPES)[number];

const PAGE_TYPES = [
  ...CONFIG_TYPES,
  "email-verification",
  "password-reset",
] as const;
type PageType = (typeof PAGE_TYPES)[number];

const ENTITY_TYPES = ["task", "user", "social-user"] as const;
type EntityType = (typeof ENTITY_TYPES)[number];

export function createApp(scope: ConfigType): {
  appConfigName: string;
  app: App;
} {
  const { name: appName, config: appConfig } = getAppConfig(scope);
  const app = new App(appName, appConfig);

  if (scope === "minimal") {
    return { appConfigName: appName, app };
  } else {
    app.auth(getAuthConfig("full"));
    app.client(getClientConfig("full"));
    app.server(getServerConfig("full"));
    app.emailSender(getEmailSenderConfig("full"));
    app.webSocket(getWebSocketConfig("full"));
    app.db(getDbConfig("full"));

    function addDecls(declName: string, nameAndConfigs: NamedConfig<object>[]) {
      nameAndConfigs.forEach(({ name, config }) => app[declName](name, config));
    }

    addDecls("page", getPageConfigs());
    addDecls("route", getRouteConfigs());
    addDecls("query", getQuerieConfigs());
    addDecls("action", getActionConfigs());
    addDecls("crud", getCrudConfigs());
    addDecls("apiNamespace", getApiNamespaceConfigs());
    addDecls("api", getApiConfigs());
    addDecls("job", getJobConfigs());

    return { appConfigName: appName, app };
  }
}

export function getAppConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.AppConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApp",
      config: {
        title: "Mock App",
        wasp: { version: "^0.16.3" },
      },
    } satisfies MinimalNamedConfig<TsAppSpec.AppConfig>;
  } else {
    return {
      name: "FullApp",
      config: {
        title: "Mock App",
        wasp: { version: "^0.16.3" },
        head: ['<link rel="icon" href="/favicon.ico" />'],
      },
    } satisfies FullNamedConfig<TsAppSpec.AppConfig>;
  }
}

export function getAuthConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: "full"): FullConfig<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: ConfigType): Config<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: ConfigType): Config<TsAppSpec.AuthConfig> {
  if (scope === "minimal") {
    return {
      userEntity: getEntity("user"),
      onAuthFailedRedirectTo: "/login",
      methods: getAuthMethods(scope),
    } satisfies MinimalConfig<TsAppSpec.AuthConfig>;
  } else {
    return {
      userEntity: getEntity("user"),
      onAuthFailedRedirectTo: "/login",
      methods: getAuthMethods(scope),
      externalAuthEntity: getEntity("social-user"),
      onAuthSucceededRedirectTo: "/profile",
      onBeforeSignup: getExtImport(scope, "named"),
      onAfterSignup: getExtImport(scope, "named"),
      onAfterEmailVerified: getExtImport(scope, "named"),
      onBeforeOAuthRedirect: getExtImport(scope, "named"),
      onBeforeLogin: getExtImport(scope, "named"),
      onAfterLogin: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.AuthConfig>;
  }
}

export function getAuthMethods(
  scope: "minimal",
): MinimalConfig<TsAppSpec.AuthMethods>;
export function getAuthMethods(
  scope: "full",
): FullConfig<TsAppSpec.AuthMethods>;
export function getAuthMethods(
  scope: ConfigType,
): Config<TsAppSpec.AuthMethods>;
export function getAuthMethods(
  scope: ConfigType,
): Config<TsAppSpec.AuthMethods> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.AuthMethods>;
  } else {
    return {
      email: getEmailAuth(scope),
      usernameAndPassword: getUsernameAndPassword(scope),
      discord: getExternalAuth(scope),
      google: getExternalAuth(scope),
      gitHub: getExternalAuth(scope),
      keycloak: getExternalAuth(scope),
    } satisfies FullConfig<TsAppSpec.AuthMethods>;
  }
}

export function getExternalAuth(
  scope: "minimal",
): MinimalConfig<TsAppSpec.ExternalAuth>;
export function getExternalAuth(
  scope: "full",
): FullConfig<TsAppSpec.ExternalAuth>;
export function getExternalAuth(
  scope: ConfigType,
): Config<TsAppSpec.ExternalAuth>;
export function getExternalAuth(
  scope: ConfigType,
): Config<TsAppSpec.ExternalAuth> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.ExternalAuth>;
  } else {
    return {
      configFn: getExtImport(scope, "named"),
      userSignupFields: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.ExternalAuth>;
  }
}

export function getUsernameAndPassword(
  scope: "minimal",
): MinimalConfig<TsAppSpec.UsernameAndPassword>;
export function getUsernameAndPassword(
  scope: "full",
): FullConfig<TsAppSpec.UsernameAndPassword>;
export function getUsernameAndPassword(
  scope: ConfigType,
): Config<TsAppSpec.UsernameAndPassword>;
export function getUsernameAndPassword(
  scope: ConfigType,
): Config<TsAppSpec.UsernameAndPassword> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.UsernameAndPassword>;
  } else {
    return {
      userSignupFields: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.UsernameAndPassword>;
  }
}

export function getEmailAuth(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailAuth>;
export function getEmailAuth(scope: "full"): FullConfig<TsAppSpec.EmailAuth>;
export function getEmailAuth(scope: ConfigType): Config<TsAppSpec.EmailAuth>;
export function getEmailAuth(scope: ConfigType): Config<TsAppSpec.EmailAuth> {
  if (scope === "minimal") {
    return {
      fromField: getEmailFromField(scope),
      emailVerification: getEmailVerification(scope),
      passwordReset: getPasswordReset(scope),
    } satisfies MinimalConfig<TsAppSpec.EmailAuth>;
  } else {
    return {
      fromField: getEmailFromField(scope),
      emailVerification: getEmailVerification(scope),
      passwordReset: getPasswordReset(scope),
      userSignupFields: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.EmailAuth>;
  }
}

export function getPasswordReset(
  scope: "minimal",
): MinimalConfig<TsAppSpec.PasswordReset>;
export function getPasswordReset(
  scope: "full",
): FullConfig<TsAppSpec.PasswordReset>;
export function getPasswordReset(
  scope: ConfigType,
): Config<TsAppSpec.PasswordReset>;
export function getPasswordReset(
  scope: ConfigType,
): Config<TsAppSpec.PasswordReset> {
  if (scope === "minimal") {
    return {
      clientRoute: getRouteConfig("password-reset").name,
    } satisfies MinimalConfig<TsAppSpec.PasswordReset>;
  } else {
    return {
      clientRoute: getRouteConfig("password-reset").name,
      getEmailContentFn: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.PasswordReset>;
  }
}

export function getEmailVerification(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailVerification>;
export function getEmailVerification(
  scope: "full",
): FullConfig<TsAppSpec.EmailVerification>;
export function getEmailVerification(
  scope: ConfigType,
): Config<TsAppSpec.EmailVerification>;
export function getEmailVerification(
  scope: ConfigType,
): Config<TsAppSpec.EmailVerification> {
  if (scope === "minimal") {
    return {
      clientRoute: getRouteConfig("email-verification").name,
    } satisfies MinimalConfig<TsAppSpec.EmailVerification>;
  } else {
    return {
      clientRoute: getRouteConfig("email-verification").name,
      getEmailContentFn: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.EmailVerification>;
  }
}

export function getClientConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.ClientConfig>;
export function getClientConfig(
  scope: "full",
): FullConfig<TsAppSpec.ClientConfig>;
export function getClientConfig(
  scope: ConfigType,
): Config<TsAppSpec.ClientConfig>;
export function getClientConfig(
  scope: ConfigType,
): Config<TsAppSpec.ClientConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.ClientConfig>;
  } else {
    return {
      rootComponent: getExtImport(scope, "named"),
      setupFn: getExtImport(scope, "named"),
      baseDir: "/src",
      envValidationSchema: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.ClientConfig>;
  }
}

export function getServerConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.ServerConfig>;
export function getServerConfig(
  scope: "full",
): FullConfig<TsAppSpec.ServerConfig>;
export function getServerConfig(
  scope: ConfigType,
): Config<TsAppSpec.ServerConfig>;
export function getServerConfig(
  scope: ConfigType,
): Config<TsAppSpec.ServerConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.ServerConfig>;
  } else {
    return {
      setupFn: getExtImport(scope, "named"),
      middlewareConfigFn: getExtImport(scope, "named"),
      envValidationSchema: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.ServerConfig>;
  }
}

export function getEmailSenderConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailSenderConfig>;
export function getEmailSenderConfig(
  scope: "full",
): FullConfig<TsAppSpec.EmailSenderConfig>;
export function getEmailSenderConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailSenderConfig>;
export function getEmailSenderConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailSenderConfig> {
  if (scope === "minimal") {
    return {
      provider: "SMTP",
    } satisfies MinimalConfig<TsAppSpec.EmailSenderConfig>;
  } else {
    return {
      provider: "SMTP",
      defaultFrom: getEmailFromField(scope),
    } satisfies FullConfig<TsAppSpec.EmailSenderConfig>;
  }
}

export function getWebSocketConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.WebsocketConfig>;
export function getWebSocketConfig(
  scope: "full",
): FullConfig<TsAppSpec.WebsocketConfig>;
export function getWebSocketConfig(
  scope: ConfigType,
): Config<TsAppSpec.WebsocketConfig>;
export function getWebSocketConfig(
  scope: ConfigType,
): Config<TsAppSpec.WebsocketConfig> {
  if (scope === "minimal") {
    return {
      fn: getExtImport(scope, "named"),
    } satisfies MinimalConfig<TsAppSpec.WebsocketConfig>;
  } else {
    return {
      fn: getExtImport(scope, "named"),
      autoConnect: true,
    } satisfies FullConfig<TsAppSpec.WebsocketConfig>;
  }
}

export function getDbConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.DbConfig>;
export function getDbConfig(scope: "full"): FullConfig<TsAppSpec.DbConfig>;
export function getDbConfig(scope: ConfigType): Config<TsAppSpec.DbConfig>;
export function getDbConfig(scope: ConfigType): Config<TsAppSpec.DbConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.DbConfig>;
  } else {
    return {
      seeds: [getExtImport(scope, "named"), getExtImport(scope, "default")],
      prismaSetupFn: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.DbConfig>;
  }
}

export function getPageConfigs(): NamedConfig<TsAppSpec.PageConfig>[] {
  return PAGE_TYPES.map((pageType) => getPageConfig(pageType));
}

export function getPageConfig(
  pageType: "minimal",
): MinimalNamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: "full" | "email-verification" | "password-reset",
): FullNamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: PageType,
): NamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: PageType,
): NamedConfig<TsAppSpec.PageConfig> {
  if (pageType === "minimal") {
    return {
      name: "MinimalPage",
      config: {
        component: getExtImport(pageType, "named"),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.PageConfig>;
  } else if (pageType === "email-verification") {
    return {
      name: "EmailVerificationPage",
      config: {
        component: getExtImport("full", "named"),
        authRequired: false,
      },
    } satisfies FullNamedConfig<TsAppSpec.PageConfig>;
  } else if (pageType === "password-reset") {
    return {
      name: "PasswordResetPage",
      config: {
        component: getExtImport("full", "named"),
        authRequired: false,
      },
    } satisfies FullNamedConfig<TsAppSpec.PageConfig>;
  } else {
    return {
      name: "FullPage",
      config: {
        component: getExtImport(pageType, "named"),
        authRequired: true,
      },
    } satisfies NamedConfig<TsAppSpec.PageConfig>;
  }
}

export function getRouteConfigs(): NamedConfig<TsAppSpec.RouteConfig>[] {
  return PAGE_TYPES.map((pageType) => getRouteConfig(pageType));
}

export function getRouteConfig(
  routeType: "minimal",
): MinimalNamedConfig<TsAppSpec.RouteConfig>;
export function getRouteConfig(
  routeType: "full" | "email-verification" | "password-reset",
): FullNamedConfig<TsAppSpec.RouteConfig>;
export function getRouteConfig(
  routeType: PageType,
): NamedConfig<TsAppSpec.RouteConfig>;
export function getRouteConfig(
  routeType: PageType,
): NamedConfig<TsAppSpec.RouteConfig> {
  if (routeType === "minimal") {
    return {
      name: "MinimalRoute",
      config: {
        path: "/foo/bar",
        to: getPageConfig(routeType).name as TsAppSpec.PageName,
      },
    } satisfies MinimalNamedConfig<TsAppSpec.RouteConfig>;
  } else {
    let name: string;
    if (routeType === "email-verification") {
      name = "EmailVerificationRoute";
    } else if (routeType === "password-reset") {
      name = "PasswordResetRoute";
    } else {
      name = "FullRoute";
    }

    return {
      name,
      config: {
        path: "/foo/bar",
        to: getPageConfig(routeType).name as TsAppSpec.PageName,
      },
    } satisfies FullNamedConfig<TsAppSpec.RouteConfig>;
  }
}

export function getQuerieConfigs(): NamedConfig<TsAppSpec.QueryConfig>[] {
  return CONFIG_TYPES.map((scope) => getQueryConfig(scope));
}

export function getQueryConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.QueryConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalQuery",
      config: {
        fn: getExtImport(scope, "named"),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.QueryConfig>;
  } else {
    return {
      name: "FullQuery",
      config: {
        fn: getExtImport(scope, "named"),
        entities: [getEntity("task")],
        auth: true,
      },
    } satisfies FullNamedConfig<TsAppSpec.QueryConfig>;
  }
}

export function getActionConfigs(): NamedConfig<TsAppSpec.ActionConfig>[] {
  return CONFIG_TYPES.map((scope) => getActionConfig(scope));
}

export function getActionConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.ActionConfig>;
export function getActionConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.ActionConfig>;
export function getActionConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ActionConfig>;
export function getActionConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ActionConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalAction",
      config: {
        fn: getExtImport(scope, "named"),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ActionConfig>;
  } else {
    return {
      name: "FullAction",
      config: {
        fn: getExtImport(scope, "named"),
        entities: [getEntity("task")],
        auth: true,
      },
    } satisfies FullNamedConfig<TsAppSpec.ActionConfig>;
  }
}

export function getCrudConfigs(): NamedConfig<TsAppSpec.CrudConfig>[] {
  return CONFIG_TYPES.map((scope) => getCrudConfig(scope));
}

export function getCrudConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.CrudConfig>;
export function getCrudConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.CrudConfig>;
export function getCrudConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.CrudConfig>;
export function getCrudConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.CrudConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalCrud",
      config: {
        entity: getEntity("task"),
        operations: getCrudOperations(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.CrudConfig>;
  } else {
    return {
      name: "FullCrud",
      config: {
        entity: getEntity("task"),
        operations: getCrudOperations(scope),
      },
    } satisfies FullNamedConfig<TsAppSpec.CrudConfig>;
  }
}

export function getCrudOperations(
  scope: "minimal",
): MinimalConfig<TsAppSpec.CrudOperations>;
export function getCrudOperations(
  scope: "full",
): FullConfig<TsAppSpec.CrudOperations>;
export function getCrudOperations(
  scope: ConfigType,
): Config<TsAppSpec.CrudOperations>;
export function getCrudOperations(
  scope: ConfigType,
): Config<TsAppSpec.CrudOperations> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.CrudOperations>;
  } else {
    return {
      get: getCrudOperationOptions(scope),
      getAll: getCrudOperationOptions(scope),
      create: getCrudOperationOptions(scope),
      update: getCrudOperationOptions(scope),
      delete: getCrudOperationOptions(scope),
    } satisfies FullConfig<TsAppSpec.CrudOperations>;
  }
}

export function getCrudOperationOptions(
  scope: "minimal",
): MinimalConfig<TsAppSpec.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: "full",
): FullConfig<TsAppSpec.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: ConfigType,
): Config<TsAppSpec.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: ConfigType,
): Config<TsAppSpec.CrudOperationOptions> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<TsAppSpec.CrudOperationOptions>;
  } else {
    return {
      isPublic: true,
      overrideFn: getExtImport(scope, "named"),
    } satisfies FullConfig<TsAppSpec.CrudOperationOptions>;
  }
}

export function getSchedule(
  scope: "minimal",
): MinimalConfig<TsAppSpec.Schedule>;
export function getSchedule(scope: "full"): FullConfig<TsAppSpec.Schedule>;
export function getSchedule(scope: ConfigType): Config<TsAppSpec.Schedule>;
export function getSchedule(scope: ConfigType): Config<TsAppSpec.Schedule> {
  if (scope === "minimal") {
    return {
      cron: "0 0 * * *",
    } satisfies MinimalConfig<TsAppSpec.Schedule>;
  } else {
    return {
      cron: "0 0 * * *",
      args: { foo: "bar" },
      executorOptions: {
        pgBoss: { jobOptions: { attempts: 3 } },
      },
    } satisfies FullConfig<TsAppSpec.Schedule>;
  }
}

export function getPerform(scope: "minimal"): MinimalConfig<TsAppSpec.Perform>;
export function getPerform(scope: "full"): FullConfig<TsAppSpec.Perform>;
export function getPerform(scope: ConfigType): Config<TsAppSpec.Perform>;
export function getPerform(scope: ConfigType): Config<TsAppSpec.Perform> {
  if (scope === "minimal") {
    return {
      fn: getExtImport(scope, "named"),
    } satisfies MinimalConfig<TsAppSpec.Perform>;
  } else {
    return {
      fn: getExtImport(scope, "named"),
      executorOptions: {
        pgBoss: { jobOptions: { attempts: 3 } },
      },
    } satisfies FullConfig<TsAppSpec.Perform>;
  }
}

export function getApiNamespaceConfigs(): NamedConfig<TsAppSpec.ApiNamespaceConfig>[] {
  return CONFIG_TYPES.map((scope) => getApiNamespaceConfig(scope));
}

export function getApiNamespaceConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.ApiNamespaceConfig>;
export function getApiNamespaceConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.ApiNamespaceConfig>;
export function getApiNamespaceConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ApiNamespaceConfig>;
export function getApiNamespaceConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ApiNamespaceConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApiNamespace",
      config: {
        middlewareConfigFn: getExtImport(scope, "named"),
        path: "/foo",
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ApiNamespaceConfig>;
  } else {
    return {
      name: "FullApiNamespace",
      config: {
        middlewareConfigFn: getExtImport(scope, "named"),
        path: "/foo",
      },
    } satisfies FullNamedConfig<TsAppSpec.ApiNamespaceConfig>;
  }
}

export function getApiConfigs(): NamedConfig<TsAppSpec.ApiConfig>[] {
  return CONFIG_TYPES.map((scope) => getApiConfig(scope));
}

export function getApiConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.ApiConfig>;
export function getApiConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.ApiConfig>;
export function getApiConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ApiConfig>;
export function getApiConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.ApiConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApi",
      config: {
        fn: getExtImport(scope, "named"),
        httpRoute: getHttpRoute(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ApiConfig>;
  } else {
    return {
      name: "FullApi",
      config: {
        fn: getExtImport(scope, "named"),
        httpRoute: getHttpRoute(scope),
        entities: [getEntity("task")],
        auth: true,
        middlewareConfigFn: getExtImport(scope, "named"),
      },
    } satisfies FullNamedConfig<TsAppSpec.ApiConfig>;
  }
}

export function getHttpRoute(
  scope: "minimal",
): MinimalConfig<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: "full"): FullConfig<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: ConfigType): Config<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: ConfigType): Config<TsAppSpec.HttpRoute> {
  if (scope === "minimal") {
    return {
      method: "GET",
      route: "/foo/bar",
    } satisfies MinimalConfig<TsAppSpec.HttpRoute>;
  } else {
    return {
      method: "GET",
      route: "/foo/bar",
    } satisfies FullConfig<TsAppSpec.HttpRoute>;
  }
}

export function getJobConfigs(): NamedConfig<TsAppSpec.JobConfig>[] {
  return CONFIG_TYPES.map((scope) => getJobConfig(scope));
}

export function getJobConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.JobConfig>;
export function getJobConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.JobConfig>;
export function getJobConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.JobConfig>;
export function getJobConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.JobConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalJob",
      config: {
        executor: "PgBoss",
        perform: getPerform(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.JobConfig>;
  } else {
    return {
      name: "FullJob",
      config: {
        executor: "PgBoss",
        perform: getPerform(scope),
        entities: [getEntity("task")],
        schedule: getSchedule(scope),
      },
    } satisfies FullNamedConfig<TsAppSpec.JobConfig>;
  }
}

export function getEmailFromField(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailFromField>;
export function getEmailFromField(
  scope: "full",
): FullConfig<TsAppSpec.EmailFromField>;
export function getEmailFromField(
  scope: ConfigType,
): Config<TsAppSpec.EmailFromField>;
export function getEmailFromField(
  scope: ConfigType,
): Config<TsAppSpec.EmailFromField> {
  if (scope === "minimal") {
    return {
      email: "test@domain.tld",
    } satisfies MinimalConfig<TsAppSpec.EmailFromField>;
  } else {
    return {
      email: "test@domain.tld",
      name: "ToDo App",
    } satisfies FullConfig<TsAppSpec.EmailFromField>;
  }
}

export function getExtImport(
  scope: "minimal",
  type: AppSpec.ExtImportKind,
): MinimalConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: "full",
  type: AppSpec.ExtImportKind,
): FullConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport> {
  if (type === "default") {
    if (scope === "minimal") {
      return {
        from: "@src/external",
        importDefault: "defaultExport",
      } satisfies MinimalConfig<TsAppSpec.ExtImport>;
    } else {
      return {
        from: "@src/external",
        importDefault: "defaultExport",
      } satisfies FullConfig<TsAppSpec.ExtImport>;
    }
  } else if (type === "named") {
    if (scope === "minimal") {
      return {
        from: "@src/external",
        import: "namedExport",
      } satisfies MinimalConfig<TsAppSpec.ExtImport>;
    } else {
      return {
        from: "@src/external",
        import: "namedExport",
      } satisfies FullConfig<TsAppSpec.ExtImport>;
    }
  } else {
    throw new Error(`Unhandled scope or type: scope=${scope}, type=${type}`);
  }
}

export function getEntity(entity: EntityType) {
  switch (entity) {
    case "task":
      return "Task";
    case "user":
      return "User";
    case "social-user":
      return "SocialUser";
    default:
      throw new Error(`Unknown entity: ${entity}`);
  }
}

export function getEntities(scope: ConfigType): string[] {
  if (scope === "minimal") {
    return [];
  }

  return ENTITY_TYPES.map((entity) => getEntity(entity));
}
