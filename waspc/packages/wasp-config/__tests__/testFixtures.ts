/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents TsAppSpec data.
 */

import * as AppSpec from "../src/appSpec.js";
import { Branded } from "../src/branded.js";
import { App } from "../src/publicApi/App.js";
import * as TsAppSpec from "../src/publicApi/tsAppSpec.js";

export function createApp(scope: ConfigType): {
  appConfigName: string;
  app: App;
} {
  const { name: appName, config: appConfig } = getAppConfig(scope);
  const app = new App(appName, appConfig);

  switch (scope) {
    case "minimal":
      return { appConfigName: appName, app };
    case "full": {
      app.auth(getAuthConfig("full"));
      app.client(getClientConfig("full"));
      app.server(getServerConfig("full"));
      app.emailSender(getEmailSenderConfig("full"));
      app.webSocket(getWebSocketConfig("full"));
      app.db(getDbConfig("full"));

      type NamedDeclMethodNameToNamedConfig = {
        [K in keyof App as Parameters<App[K]> extends [string, object]
          ? K
          : never]: NamedConfig<Parameters<App[K]>[1]>;
      };

      /**
       * We thinks this may be a false positive
       * @see https://www.typescriptlang.org/play/?#code/LAKAxgNghgzjAEAZKBzKA7A9vA3qe8ADgE4BuUAFAC4AWAljAFxKoaYA08AHszFcXXQoAlM1KY6AE1wBffPEnEArmmr0mLNFk4946JQFsARgFNio+OKmzQckKCoBPQifgA5KAZMAVTN+euALy48gDaANLwgvAA1iaOmABmmmwAuswAClDEniZUZjAAPMhamBGpAHyhAAyptqCgYJjofPDQWoJ08MHoJgDuKVgUwgDcDSCJSuhgVHTN8PkwMZiFALLwJlz56JIIcQnJHl6+-i4VFOi5zKuc2SjMRz5+AaGrqRZW0nggBO0YnaFLl5UhQ7sJ4AB6CHwABWZngfAEQngADI9IZTMRunoTKQzPUQEA
       */
      function addNamedDecl<M extends keyof NamedDeclMethodNameToNamedConfig>({
        declName,
        namedConfigs,
      }: {
        declName: M;
        namedConfigs: NamedDeclMethodNameToNamedConfig[M][];
      }): void {
        namedConfigs.forEach(({ name, config }) =>
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          app[declName](name, config as any),
        );
      }

      addNamedDecl({
        declName: "page",
        namedConfigs: getPageConfigs(),
      });
      addNamedDecl({
        declName: "route",
        namedConfigs: getRouteConfigs(),
      });
      addNamedDecl({
        declName: "query",
        namedConfigs: getQueryConfigs(),
      });
      addNamedDecl({
        declName: "action",
        namedConfigs: getActionConfigs(),
      });
      addNamedDecl({
        declName: "crud",
        namedConfigs: getCrudConfigs(),
      });
      addNamedDecl({
        declName: "apiNamespace",
        namedConfigs: getApiNamespaceConfigs(),
      });
      addNamedDecl({
        declName: "api",
        namedConfigs: getApiConfigs(),
      });
      addNamedDecl({
        declName: "job",
        namedConfigs: getJobConfigs(),
      });

      return { appConfigName: appName, app };
    }
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalApp",
        config: {
          title: "Mock App",
          wasp: { version: "^0.16.3" },
        },
      } satisfies MinimalNamedConfig<TsAppSpec.AppConfig>;
    case "full":
      return {
        name: "FullApp",
        config: {
          title: "Mock App",
          wasp: { version: "^0.16.3" },
          head: ['<link rel="icon" href="/favicon.ico" />'],
        },
      } satisfies FullNamedConfig<TsAppSpec.AppConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getAuthConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: "full"): FullConfig<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: ConfigType): Config<TsAppSpec.AuthConfig>;
export function getAuthConfig(scope: ConfigType): Config<TsAppSpec.AuthConfig> {
  switch (scope) {
    case "minimal":
      return {
        userEntity: getEntity("user"),
        onAuthFailedRedirectTo: "/login",
        methods: getAuthMethods(scope),
      } satisfies MinimalConfig<TsAppSpec.AuthConfig>;
    case "full":
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
        sessionLength: 30 * 24 * 60 * 60 * 1000, // 30 days
      } satisfies FullConfig<TsAppSpec.AuthConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.AuthMethods>;
    case "full":
      return {
        email: getEmailAuthConfig(scope),
        usernameAndPassword: getUsernameAndPasswordConfig(scope),
        discord: getExternalAuthConfig(scope),
        google: getExternalAuthConfig(scope),
        gitHub: getExternalAuthConfig(scope),
        keycloak: getExternalAuthConfig(scope),
      } satisfies FullConfig<TsAppSpec.AuthMethods>;
    default:
      assertUnreachable(scope);
  }
}

export function getExternalAuthConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.ExternalAuthConfig>;
export function getExternalAuthConfig(
  scope: "full",
): FullConfig<TsAppSpec.ExternalAuthConfig>;
export function getExternalAuthConfig(
  scope: ConfigType,
): Config<TsAppSpec.ExternalAuthConfig>;
export function getExternalAuthConfig(
  scope: ConfigType,
): Config<TsAppSpec.ExternalAuthConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.ExternalAuthConfig>;
    case "full":
      return {
        configFn: getExtImport(scope, "named"),
        userSignupFields: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.ExternalAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getUsernameAndPasswordConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.UsernameAndPasswordConfig>;
export function getUsernameAndPasswordConfig(
  scope: "full",
): FullConfig<TsAppSpec.UsernameAndPasswordConfig>;
export function getUsernameAndPasswordConfig(
  scope: ConfigType,
): Config<TsAppSpec.UsernameAndPasswordConfig>;
export function getUsernameAndPasswordConfig(
  scope: ConfigType,
): Config<TsAppSpec.UsernameAndPasswordConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.UsernameAndPasswordConfig>;
    case "full":
      return {
        userSignupFields: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.UsernameAndPasswordConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailAuthConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailAuthConfig>;
export function getEmailAuthConfig(
  scope: "full",
): FullConfig<TsAppSpec.EmailAuthConfig>;
export function getEmailAuthConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailAuthConfig>;
export function getEmailAuthConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailAuthConfig> {
  switch (scope) {
    case "minimal":
      return {
        fromField: getEmailFromField(scope),
        emailVerification: getEmailVerificationConfig(scope),
        passwordReset: getPasswordResetConfig(scope),
      } satisfies MinimalConfig<TsAppSpec.EmailAuthConfig>;
    case "full":
      return {
        fromField: getEmailFromField(scope),
        emailVerification: getEmailVerificationConfig(scope),
        passwordReset: getPasswordResetConfig(scope),
        userSignupFields: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.EmailAuthConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getPasswordResetConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.PasswordResetConfig>;
export function getPasswordResetConfig(
  scope: "full",
): FullConfig<TsAppSpec.PasswordResetConfig>;
export function getPasswordResetConfig(
  scope: ConfigType,
): Config<TsAppSpec.PasswordResetConfig>;
export function getPasswordResetConfig(
  scope: ConfigType,
): Config<TsAppSpec.PasswordResetConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: getRouteConfig("password-reset").name,
      } satisfies MinimalConfig<TsAppSpec.PasswordResetConfig>;
    case "full":
      return {
        clientRoute: getRouteConfig("password-reset").name,
        getEmailContentFn: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.PasswordResetConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getEmailVerificationConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.EmailVerificationConfig>;
export function getEmailVerificationConfig(
  scope: "full",
): FullConfig<TsAppSpec.EmailVerificationConfig>;
export function getEmailVerificationConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailVerificationConfig>;
export function getEmailVerificationConfig(
  scope: ConfigType,
): Config<TsAppSpec.EmailVerificationConfig> {
  switch (scope) {
    case "minimal":
      return {
        clientRoute: getRouteConfig("email-verification").name,
      } satisfies MinimalConfig<TsAppSpec.EmailVerificationConfig>;
    case "full":
      return {
        clientRoute: getRouteConfig("email-verification").name,
        getEmailContentFn: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.EmailVerificationConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.ClientConfig>;
    case "full":
      return {
        rootComponent: getExtImport(scope, "named"),
        setupFn: getExtImport(scope, "named"),
        baseDir: "/src",
        envValidationSchema: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.ClientConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.ServerConfig>;
    case "full":
      return {
        setupFn: getExtImport(scope, "named"),
        middlewareConfigFn: getExtImport(scope, "named"),
        envValidationSchema: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.ServerConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {
        provider: "SMTP",
      } satisfies MinimalConfig<TsAppSpec.EmailSenderConfig>;
    case "full":
      return {
        provider: "SMTP",
        defaultFrom: getEmailFromField(scope),
      } satisfies FullConfig<TsAppSpec.EmailSenderConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {
        fn: getExtImport(scope, "named"),
      } satisfies MinimalConfig<TsAppSpec.WebsocketConfig>;
    case "full":
      return {
        fn: getExtImport(scope, "named"),
        autoConnect: true,
      } satisfies FullConfig<TsAppSpec.WebsocketConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getDbConfig(
  scope: "minimal",
): MinimalConfig<TsAppSpec.DbConfig>;
export function getDbConfig(scope: "full"): FullConfig<TsAppSpec.DbConfig>;
export function getDbConfig(scope: ConfigType): Config<TsAppSpec.DbConfig>;
export function getDbConfig(scope: ConfigType): Config<TsAppSpec.DbConfig> {
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.DbConfig>;
    case "full":
      return {
        seeds: [getExtImport(scope, "named"), getExtImport(scope, "default")],
        prismaSetupFn: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.DbConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getPageConfigs(): NamedConfig<TsAppSpec.PageConfig>[] {
  return PAGE_CONFIG_TYPES.map(getPageConfig);
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
  const name = `${pageType}-page`;
  switch (pageType) {
    case "minimal":
      return {
        name,
        config: {
          component: getExtImport(pageType, "named"),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.PageConfig>;
    case "email-verification":
      return {
        name,
        config: {
          component: getExtImport("full", "named"),
          authRequired: false,
        },
      } satisfies FullNamedConfig<TsAppSpec.PageConfig>;
    case "password-reset":
      return {
        name,
        config: {
          component: getExtImport("full", "named"),
          authRequired: false,
        },
      } satisfies FullNamedConfig<TsAppSpec.PageConfig>;
    case "full":
      return {
        name,
        config: {
          component: getExtImport(pageType, "named"),
          authRequired: true,
        },
      } satisfies NamedConfig<TsAppSpec.PageConfig>;
    default:
      assertUnreachable(pageType);
  }
}

export function getRouteConfigs(): NamedConfig<TsAppSpec.RouteConfig>[] {
  return PAGE_CONFIG_TYPES.map(getRouteConfig);
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
  const name = `${routeType}-route`;
  switch (routeType) {
    case "minimal":
      return {
        name,
        config: {
          path: "/foo/bar",
          to: getPageConfig(routeType).name as TsAppSpec.PageName,
        },
      } satisfies MinimalNamedConfig<TsAppSpec.RouteConfig>;
    case "full":
    case "email-verification":
    case "password-reset":
      return {
        name,
        config: {
          path: "/foo/bar",
          to: getPageConfig(routeType).name as TsAppSpec.PageName,
        },
      } satisfies FullNamedConfig<TsAppSpec.RouteConfig>;
    default:
      assertUnreachable(routeType);
  }
}

export function getQueryConfigs(): NamedConfig<TsAppSpec.QueryConfig>[] {
  return CONFIG_TYPES.map(getQueryConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalQuery",
        config: {
          fn: getExtImport(scope, "named"),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.QueryConfig>;
    case "full":
      return {
        name: "FullQuery",
        config: {
          fn: getExtImport(scope, "named"),
          entities: [getEntity("task")],
          auth: true,
        },
      } satisfies FullNamedConfig<TsAppSpec.QueryConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getActionConfigs(): NamedConfig<TsAppSpec.ActionConfig>[] {
  return CONFIG_TYPES.map(getActionConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalAction",
        config: {
          fn: getExtImport(scope, "named"),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.ActionConfig>;
    case "full":
      return {
        name: "FullAction",
        config: {
          fn: getExtImport(scope, "named"),
          entities: [getEntity("task")],
          auth: true,
        },
      } satisfies FullNamedConfig<TsAppSpec.ActionConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getCrudConfigs(): NamedConfig<TsAppSpec.CrudConfig>[] {
  return CONFIG_TYPES.map(getCrudConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalCrud",
        config: {
          entity: getEntity("task"),
          operations: getCrudOperations(scope),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.CrudConfig>;
    case "full":
      return {
        name: "FullCrud",
        config: {
          entity: getEntity("task"),
          operations: getCrudOperations(scope),
        },
      } satisfies FullNamedConfig<TsAppSpec.CrudConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.CrudOperations>;
    case "full":
      return {
        get: getCrudOperationOptions(scope),
        getAll: getCrudOperationOptions(scope),
        create: getCrudOperationOptions(scope),
        update: getCrudOperationOptions(scope),
        delete: getCrudOperationOptions(scope),
      } satisfies FullConfig<TsAppSpec.CrudOperations>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {} satisfies MinimalConfig<TsAppSpec.CrudOperationOptions>;
    case "full":
      return {
        isPublic: true,
        overrideFn: getExtImport(scope, "named"),
      } satisfies FullConfig<TsAppSpec.CrudOperationOptions>;
    default:
      assertUnreachable(scope);
  }
}

export function getSchedule(
  scope: "minimal",
): MinimalConfig<TsAppSpec.Schedule>;
export function getSchedule(scope: "full"): FullConfig<TsAppSpec.Schedule>;
export function getSchedule(scope: ConfigType): Config<TsAppSpec.Schedule>;
export function getSchedule(scope: ConfigType): Config<TsAppSpec.Schedule> {
  switch (scope) {
    case "minimal":
      return {
        cron: "0 0 * * *",
      } satisfies MinimalConfig<TsAppSpec.Schedule>;
    case "full":
      return {
        cron: "0 0 * * *",
        args: { foo: "bar" },
        executorOptions: {
          pgBoss: { jobOptions: { attempts: 3 } },
        },
      } satisfies FullConfig<TsAppSpec.Schedule>;
    default:
      assertUnreachable(scope);
  }
}

export function getPerform(scope: "minimal"): MinimalConfig<TsAppSpec.Perform>;
export function getPerform(scope: "full"): FullConfig<TsAppSpec.Perform>;
export function getPerform(scope: ConfigType): Config<TsAppSpec.Perform>;
export function getPerform(scope: ConfigType): Config<TsAppSpec.Perform> {
  switch (scope) {
    case "minimal":
      return {
        fn: getExtImport(scope, "named"),
      } satisfies MinimalConfig<TsAppSpec.Perform>;
    case "full":
      return {
        fn: getExtImport(scope, "named"),
        executorOptions: {
          pgBoss: { jobOptions: { attempts: 3 } },
        },
      } satisfies FullConfig<TsAppSpec.Perform>;
    default:
      assertUnreachable(scope);
  }
}

export function getApiNamespaceConfigs(): NamedConfig<TsAppSpec.ApiNamespaceConfig>[] {
  return CONFIG_TYPES.map(getApiNamespaceConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalApiNamespace",
        config: {
          middlewareConfigFn: getExtImport(scope, "named"),
          path: "/foo",
        },
      } satisfies MinimalNamedConfig<TsAppSpec.ApiNamespaceConfig>;
    case "full":
      return {
        name: "FullApiNamespace",
        config: {
          middlewareConfigFn: getExtImport(scope, "named"),
          path: "/foo",
        },
      } satisfies FullNamedConfig<TsAppSpec.ApiNamespaceConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getApiConfigs(): NamedConfig<TsAppSpec.ApiConfig>[] {
  return CONFIG_TYPES.map(getApiConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalApi",
        config: {
          fn: getExtImport(scope, "named"),
          httpRoute: getHttpRoute(scope),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.ApiConfig>;
    case "full":
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
    default:
      assertUnreachable(scope);
  }
}

export function getHttpRoute(
  scope: "minimal",
): MinimalConfig<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: "full"): FullConfig<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: ConfigType): Config<TsAppSpec.HttpRoute>;
export function getHttpRoute(scope: ConfigType): Config<TsAppSpec.HttpRoute> {
  switch (scope) {
    case "minimal":
      return {
        method: "GET",
        route: "/foo/bar",
      } satisfies MinimalConfig<TsAppSpec.HttpRoute>;
    case "full":
      return {
        method: "GET",
        route: "/foo/bar",
      } satisfies FullConfig<TsAppSpec.HttpRoute>;
    default:
      assertUnreachable(scope);
  }
}

export function getJobConfigs(): NamedConfig<TsAppSpec.JobConfig>[] {
  return CONFIG_TYPES.map(getJobConfig);
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
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalJob",
        config: {
          executor: "PgBoss",
          perform: getPerform(scope),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.JobConfig>;
    case "full":
      return {
        name: "FullJob",
        config: {
          executor: "PgBoss",
          perform: getPerform(scope),
          entities: [getEntity("task")],
          schedule: getSchedule(scope),
        },
      } satisfies FullNamedConfig<TsAppSpec.JobConfig>;
    default:
      assertUnreachable(scope);
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
  switch (scope) {
    case "minimal":
      return {
        email: "test@domain.tld",
      } satisfies MinimalConfig<TsAppSpec.EmailFromField>;
    case "full":
      return {
        email: "test@domain.tld",
        name: "ToDo App",
      } satisfies FullConfig<TsAppSpec.EmailFromField>;
    default:
      assertUnreachable(scope);
  }
}

export function getExtImport(
  scope: "minimal",
  importKind: AppSpec.ExtImportKind,
): MinimalConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: "full",
  importKind: AppSpec.ExtImportKind,
): FullConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport> {
  const importObject =
    importKind === "default"
      ? { importDefault: "defaultExport" }
      : { import: "namedExport" };

  switch (scope) {
    case "minimal":
      return {
        ...importObject,
        from: "@src/external",
      } satisfies MinimalConfig<TsAppSpec.ExtImport>;
    case "full":
      return {
        ...importObject,
        from: "@src/external",
      } satisfies FullConfig<TsAppSpec.ExtImport>;
    default:
      assertUnreachable(scope);
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
      assertUnreachable(entity);
  }
}

export function getEntities(scope: ConfigType): string[] {
  switch (scope) {
    case "minimal":
      return [];
    case "full":
      return ENTITY_CONFIG_TYPES.map(getEntity);
    default:
      assertUnreachable(scope);
  }
}

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}

const CONFIG_TYPES = ["minimal", "full"] as const;
type ConfigType = (typeof CONFIG_TYPES)[number];

/**
 * By default we define only `ConfigType` variants for all of the configs
 * that can be used in the app. This is because we want to test both
 * edge cases of the configs.
 *
 * Pages are a special case, because even though they are a top-level config,
 * they are also used by `AuthConfig`. That is why we define those two cases separately.
 * Mostly to bring attention that we have additional edge cases for pages.
 */
const PAGE_CONFIG_TYPES = [
  ...CONFIG_TYPES,
  "email-verification",
  "password-reset",
] as const;
type PageType = (typeof PAGE_CONFIG_TYPES)[number];

const ENTITY_CONFIG_TYPES = ["task", "user", "social-user"] as const;
type EntityType = (typeof ENTITY_CONFIG_TYPES)[number];

type NamedConfig<T> = MinimalNamedConfig<T> | FullNamedConfig<T>;
export type Config<T> = MinimalConfig<T> | FullConfig<T>;

type MinimalNamedConfig<T> = {
  name: string;
  config: MinimalConfig<T>;
};

type FullNamedConfig<T> = {
  name: string;
  config: FullConfig<T>;
};

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
export type MinimalConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer ArrayItem>
      ? Array<MinimalConfig<ArrayItem>>
      : T extends object
        ? keyof T extends never
          ? EmptyObject
          : MinimalConfigObject<T>
        : T;

type MinimalConfigObject<T> = {
  [K in keyof T as EmptyObject extends Pick<T, K> ? never : K]: MinimalConfig<
    T[K]
  >;
} extends infer Object
  ? Object extends EmptyObject
    ? EmptyObject
    : Object
  : never;

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
export type FullConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer ArrayItem>
      ? Array<FullConfig<ArrayItem>>
      : T extends object
        ? FullConfigObject<T>
        : T;

type FullConfigObject<T> = {
  [K in keyof T]-?: FullConfig<T[K]>;
};
