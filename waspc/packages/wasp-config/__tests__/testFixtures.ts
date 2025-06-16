/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents UserSpec data.
 */

import * as AppSpec from "../src/appSpec.js";
import { Branded } from "../src/branded.js";
import * as UserApi from "../src/userApi.js";

/**
 * Creates a type containing only the required properties from T.
 *
 * This utility:
 * - Filters out optional properties from the type
 * - Provides a clean type for minimal configuration objects
 * - Returns `Record<string, never>` (empty object) when no required properties exist
 *
 * @template T - The type to extract required properties from
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
type MinimalConfig<T> = keyof T extends never
  ? Record<string, never>
  : {
        [K in keyof T as undefined extends T[K] ? never : K]: T[K];
      } extends infer R
    ? keyof R extends never
      ? Record<string, never>
      : R
    : never;

/**
 * Creates a type with all properties and nested properties required.
 *
 * This utility:
 * - Makes all properties required recursively (removes optional flags)
 * - Stops from unwrapping branded types fully
 *
 * @template T - The type to make fully required
 */
type FullConfig<T> =
  T extends Branded<infer U, infer B>
    ? Branded<U, B>
    : T extends object
      ? { [K in keyof T]-?: FullConfig<T[K]> }
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

export function createUserApp(scope: ConfigType): {
  appName: string;
  userApp: UserApi.App;
} {
  if (scope === "minimal") {
    const { name: appName, config: appConfig } = getApp(scope);
    const userApp = new UserApi.App(appName, appConfig);
    return { appName, userApp };
  }

  const { name: appName, config: appConfig } = getApp(scope);
  const userApp = new UserApi.App(appName, appConfig);

  userApp.auth(getAuth("full"));
  userApp.client(getClient("full"));
  userApp.server(getServer("full"));
  userApp.emailSender(getEmailSender("full"));
  userApp.webSocket(getWebSocket("full"));
  userApp.db(getDb("full"));

  function addDecls(declName: string, nameAndConfigs: NamedConfig<unknown>[]) {
    nameAndConfigs.forEach(({ name, config }) =>
      userApp[declName](name, config),
    );
  }

  addDecls("page", getPages());
  addDecls("route", getRoutes());
  addDecls("query", getQueries());
  addDecls("action", getActions());
  addDecls("crud", getCruds());
  addDecls("apiNamespace", getApiNamespaces());
  addDecls("api", getApis());
  addDecls("job", getJobs());

  return { appName, userApp };
}

export function getApp(scope: "minimal"): MinimalNamedConfig<UserApi.AppConfig>;
export function getApp(scope: "full"): FullNamedConfig<UserApi.AppConfig>;
export function getApp(scope: ConfigType): NamedConfig<UserApi.AppConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApp",
      config: {
        title: "Mock App",
        wasp: { version: "^0.16.3" },
      },
    } satisfies MinimalNamedConfig<UserApi.AppConfig>;
  }

  return {
    name: "FullApp",
    config: {
      title: "Mock App",
      wasp: { version: "^0.16.3" },
      head: ['<link rel="icon" href="/favicon.ico" />'],
    },
  } satisfies FullNamedConfig<UserApi.AppConfig>;
}

export function getAuth(scope: "minimal"): MinimalConfig<UserApi.AuthConfig>;
export function getAuth(scope: "full"): FullConfig<UserApi.AuthConfig>;
export function getAuth(scope: ConfigType): Config<UserApi.AuthConfig> {
  if (scope === "minimal") {
    return {
      userEntity: getEntity("user"),
      onAuthFailedRedirectTo: "/login",
      methods: getAuthMethods(scope),
    } satisfies MinimalConfig<UserApi.AuthConfig>;
  }

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
  } satisfies FullConfig<UserApi.AuthConfig>;
}

export function getAuthMethods(
  scope: "minimal",
): MinimalConfig<UserApi.AuthMethods>;
export function getAuthMethods(scope: "full"): FullConfig<UserApi.AuthMethods>;
export function getAuthMethods(scope: ConfigType): Config<UserApi.AuthMethods> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.AuthMethods>;
  }

  return {
    email: getEmailAuth(scope),
    usernameAndPassword: getUsernameAndPassword(scope),
    discord: getExternalAuth(scope),
    google: getExternalAuth(scope),
    gitHub: getExternalAuth(scope),
    keycloak: getExternalAuth(scope),
  } satisfies FullConfig<UserApi.AuthMethods>;
}

export function getExternalAuth(
  scope: "minimal",
): MinimalConfig<UserApi.ExternalAuthConfig>;
export function getExternalAuth(
  scope: "full",
): FullConfig<UserApi.ExternalAuthConfig>;
export function getExternalAuth(
  scope: ConfigType,
): Config<UserApi.ExternalAuthConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.ExternalAuthConfig>;
  }

  return {
    configFn: getExtImport(scope, "named"),
    userSignupFields: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.ExternalAuthConfig>;
}

export function getUsernameAndPassword(
  scope: "minimal",
): MinimalConfig<UserApi.UsernameAndPasswordConfig>;
export function getUsernameAndPassword(
  scope: "full",
): FullConfig<UserApi.UsernameAndPasswordConfig>;
export function getUsernameAndPassword(
  scope: ConfigType,
): Config<UserApi.UsernameAndPasswordConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.UsernameAndPasswordConfig>;
  }

  return {
    userSignupFields: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.UsernameAndPasswordConfig>;
}

export function getEmailAuth(
  scope: "minimal",
): MinimalConfig<UserApi.EmailAuthConfig>;
export function getEmailAuth(
  scope: "full",
): FullConfig<UserApi.EmailAuthConfig>;
export function getEmailAuth(
  scope: ConfigType,
): Config<UserApi.EmailAuthConfig> {
  if (scope === "minimal") {
    return {
      fromField: getEmailFromField(scope),
      emailVerification: getEmailVerification(scope),
      passwordReset: getPasswordReset(scope),
    } satisfies MinimalConfig<UserApi.EmailAuthConfig>;
  }

  return {
    fromField: getEmailFromField(scope),
    emailVerification: getEmailVerification(scope),
    passwordReset: getPasswordReset(scope),
    userSignupFields: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.EmailAuthConfig>;
}

export function getPasswordReset(
  scope: "minimal",
): MinimalConfig<UserApi.PasswordResetConfig>;
export function getPasswordReset(
  scope: "full",
): FullConfig<UserApi.PasswordResetConfig>;
export function getPasswordReset(
  scope: ConfigType,
): Config<UserApi.PasswordResetConfig> {
  if (scope === "minimal") {
    return {
      clientRoute: getRoute("password-reset").name,
    } satisfies MinimalConfig<UserApi.PasswordResetConfig>;
  }

  return {
    clientRoute: getRoute("password-reset").name,
    getEmailContentFn: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.PasswordResetConfig>;
}

export function getEmailVerification(
  scope: "minimal",
): MinimalConfig<UserApi.EmailVerificationConfig>;
export function getEmailVerification(
  scope: "full",
): FullConfig<UserApi.EmailVerificationConfig>;
export function getEmailVerification(
  scope: ConfigType,
): Config<UserApi.EmailVerificationConfig> {
  if (scope === "minimal") {
    return {
      clientRoute: getRoute("email-verification").name,
    } satisfies MinimalConfig<UserApi.EmailVerificationConfig>;
  }

  return {
    clientRoute: getRoute("email-verification").name,
    getEmailContentFn: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.EmailVerificationConfig>;
}

export function getClient(
  scope: "minimal",
): MinimalConfig<UserApi.ClientConfig>;
export function getClient(scope: "full"): FullConfig<UserApi.ClientConfig>;
export function getClient(scope: ConfigType): Config<UserApi.ClientConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.ClientConfig>;
  }

  return {
    rootComponent: getExtImport(scope, "named"),
    setupFn: getExtImport(scope, "named"),
    baseDir: "/src",
    envValidationSchema: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.ClientConfig>;
}

export function getServer(
  scope: "minimal",
): MinimalConfig<UserApi.ServerConfig>;
export function getServer(scope: "full"): FullConfig<UserApi.ServerConfig>;
export function getServer(scope: ConfigType): Config<UserApi.ServerConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.ServerConfig>;
  }

  return {
    setupFn: getExtImport(scope, "named"),
    middlewareConfigFn: getExtImport(scope, "named"),
    envValidationSchema: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.ServerConfig>;
}

export function getEmailSender(
  scope: "minimal",
): MinimalConfig<UserApi.EmailSenderConfig>;
export function getEmailSender(
  scope: "full",
): FullConfig<UserApi.EmailSenderConfig>;
export function getEmailSender(
  scope: ConfigType,
): Config<UserApi.EmailSenderConfig> {
  if (scope === "minimal") {
    return {
      provider: "SMTP",
    } satisfies MinimalConfig<UserApi.EmailSenderConfig>;
  }

  return {
    provider: "SMTP",
    defaultFrom: getEmailFromField(scope),
  } satisfies FullConfig<UserApi.EmailSenderConfig>;
}

export function getWebSocket(
  scope: "minimal",
): MinimalConfig<UserApi.WebsocketConfig>;
export function getWebSocket(
  scope: "full",
): FullConfig<UserApi.WebsocketConfig>;
export function getWebSocket(
  scope: ConfigType,
): Config<UserApi.WebsocketConfig> {
  if (scope === "minimal") {
    return {
      fn: getExtImport(scope, "named"),
    } satisfies MinimalConfig<UserApi.WebsocketConfig>;
  }

  return {
    fn: getExtImport(scope, "named"),
    autoConnect: true,
  } satisfies FullConfig<UserApi.WebsocketConfig>;
}

export function getDb(scope: "minimal"): MinimalConfig<UserApi.DbConfig>;
export function getDb(scope: "full"): FullConfig<UserApi.DbConfig>;
export function getDb(scope: ConfigType): Config<UserApi.DbConfig> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.DbConfig>;
  }

  return {
    seeds: [getExtImport(scope, "named"), getExtImport(scope, "default")],
    prismaSetupFn: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.DbConfig>;
}

export function getPages(): NamedConfig<UserApi.PageConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return PAGE_TYPES.map((pageType) => getPage(pageType));
}

export function getPage(
  pageType: "minimal",
): MinimalNamedConfig<UserApi.PageConfig>;
export function getPage(
  pageType: "full" | "email-verification" | "password-reset",
): FullNamedConfig<UserApi.PageConfig>;
export function getPage(pageType: PageType): NamedConfig<UserApi.PageConfig> {
  if (pageType === "minimal") {
    return {
      name: "MinimalPage",
      config: {
        component: getExtImport(pageType, "named"),
      },
    } satisfies MinimalNamedConfig<UserApi.PageConfig>;
  } else if (pageType === "email-verification") {
    return {
      name: "EmailVerificationPage",
      config: {
        component: getExtImport("full", "named"),
        authRequired: false,
      },
    } satisfies FullNamedConfig<UserApi.PageConfig>;
  } else if (pageType === "password-reset") {
    return {
      name: "PasswordResetPage",
      config: {
        component: getExtImport("full", "named"),
        authRequired: false,
      },
    } satisfies FullNamedConfig<UserApi.PageConfig>;
  }

  return {
    name: "FullPage",
    config: {
      component: getExtImport(pageType, "named"),
      authRequired: true,
    },
  } satisfies NamedConfig<UserApi.PageConfig>;
}

export function getRoutes(): NamedConfig<UserApi.RouteConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return PAGE_TYPES.map((pageType) => getRoute(pageType));
}

export function getRoute(
  routeType: "minimal",
): MinimalNamedConfig<UserApi.RouteConfig>;
export function getRoute(
  routeType: "full" | "email-verification" | "password-reset",
): FullNamedConfig<UserApi.RouteConfig>;
export function getRoute(
  routeType: PageType,
): NamedConfig<UserApi.RouteConfig> {
  if (routeType === "minimal") {
    return {
      name: "MinimalRoute",
      config: {
        path: "/foo/bar",
        to: getPage(routeType).name as UserApi.PageName,
      },
    } satisfies MinimalNamedConfig<UserApi.RouteConfig>;
  }

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
      to: getPage(routeType).name as UserApi.PageName,
    },
  } satisfies FullNamedConfig<UserApi.RouteConfig>;
}

export function getQueries(): NamedConfig<UserApi.QueryConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getQuery(scope));
}

export function getQuery(
  scope: "minimal",
): MinimalNamedConfig<UserApi.QueryConfig>;
export function getQuery(scope: "full"): FullNamedConfig<UserApi.QueryConfig>;
export function getQuery(scope: ConfigType): NamedConfig<UserApi.QueryConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalQuery",
      config: {
        fn: getExtImport(scope, "named"),
      },
    } satisfies MinimalNamedConfig<UserApi.QueryConfig>;
  }

  return {
    name: "FullQuery",
    config: {
      fn: getExtImport(scope, "named"),
      entities: [getEntity("task")],
      auth: true,
    },
  } satisfies FullNamedConfig<UserApi.QueryConfig>;
}

export function getActions(): NamedConfig<UserApi.ActionConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getAction(scope));
}

export function getAction(
  scope: "minimal",
): MinimalNamedConfig<UserApi.ActionConfig>;
export function getAction(scope: "full"): FullNamedConfig<UserApi.ActionConfig>;
export function getAction(
  scope: ConfigType,
): NamedConfig<UserApi.ActionConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalAction",
      config: {
        fn: getExtImport(scope, "named"),
      },
    } satisfies MinimalNamedConfig<UserApi.ActionConfig>;
  }

  return {
    name: "FullAction",
    config: {
      fn: getExtImport(scope, "named"),
      entities: [getEntity("task")],
      auth: true,
    },
  } satisfies FullNamedConfig<UserApi.ActionConfig>;
}

export function getCruds(): NamedConfig<UserApi.Crud>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getCrud(scope));
}

export function getCrud(scope: "minimal"): MinimalNamedConfig<UserApi.Crud>;
export function getCrud(scope: "full"): FullNamedConfig<UserApi.Crud>;
export function getCrud(scope: ConfigType): NamedConfig<UserApi.Crud> {
  if (scope === "minimal") {
    return {
      name: "MinimalCrud",
      config: {
        entity: getEntity("task"),
        operations: getCrudOperations(scope),
      },
    } satisfies MinimalNamedConfig<UserApi.Crud>;
  }

  return {
    name: "FullCrud",
    config: {
      entity: getEntity("task"),
      operations: getCrudOperations(scope),
    },
  } satisfies FullNamedConfig<UserApi.Crud>;
}

export function getCrudOperations(
  scope: "minimal",
): MinimalConfig<UserApi.CrudOperations>;
export function getCrudOperations(
  scope: "full",
): FullConfig<UserApi.CrudOperations>;
export function getCrudOperations(
  scope: ConfigType,
): Config<UserApi.CrudOperations> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.CrudOperations>;
  }

  return {
    get: getCrudOperationOptions(scope),
    getAll: getCrudOperationOptions(scope),
    create: getCrudOperationOptions(scope),
    update: getCrudOperationOptions(scope),
    delete: getCrudOperationOptions(scope),
  } satisfies FullConfig<UserApi.CrudOperations>;
}

export function getCrudOperationOptions(
  scope: "minimal",
): MinimalConfig<UserApi.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: "full",
): FullConfig<UserApi.CrudOperationOptions>;
export function getCrudOperationOptions(
  scope: ConfigType,
): Config<UserApi.CrudOperationOptions> {
  if (scope === "minimal") {
    return {} satisfies MinimalConfig<UserApi.CrudOperationOptions>;
  }

  return {
    isPublic: true,
    overrideFn: getExtImport(scope, "named"),
  } satisfies FullConfig<UserApi.CrudOperationOptions>;
}

export function getSchedule(
  scope: "minimal",
): MinimalConfig<UserApi.ScheduleConfig>;
export function getSchedule(scope: "full"): FullConfig<UserApi.ScheduleConfig>;
export function getSchedule(scope: ConfigType): Config<UserApi.ScheduleConfig> {
  if (scope === "minimal") {
    return {
      cron: "0 0 * * *",
    } satisfies MinimalConfig<UserApi.ScheduleConfig>;
  }

  return {
    cron: "0 0 * * *",
    args: { foo: "bar" },
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<UserApi.ScheduleConfig>;
}

export function getPerform(scope: "minimal"): MinimalConfig<UserApi.Perform>;
export function getPerform(scope: "full"): FullConfig<UserApi.Perform>;
export function getPerform(
  scope: ConfigType,
): MinimalConfig<UserApi.Perform> | FullConfig<UserApi.Perform> {
  if (scope === "minimal") {
    return {
      fn: getExtImport(scope, "named"),
    } satisfies MinimalConfig<UserApi.Perform>;
  }

  return {
    fn: getExtImport(scope, "named"),
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<UserApi.Perform>;
}

export function getApiNamespaces(): NamedConfig<UserApi.ApiNamespaceConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getApiNamespace(scope));
}

export function getApiNamespace(
  scope: "minimal",
): MinimalNamedConfig<UserApi.ApiNamespaceConfig>;
export function getApiNamespace(
  scope: "full",
): FullNamedConfig<UserApi.ApiNamespaceConfig>;
export function getApiNamespace(
  scope: ConfigType,
): NamedConfig<UserApi.ApiNamespaceConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApiNamespace",
      config: {
        middlewareConfigFn: getExtImport(scope, "named"),
        path: "/foo",
      },
    } satisfies MinimalNamedConfig<UserApi.ApiNamespaceConfig>;
  }

  return {
    name: "FullApiNamespace",
    config: {
      middlewareConfigFn: getExtImport(scope, "named"),
      path: "/foo",
    },
  } satisfies FullNamedConfig<UserApi.ApiNamespaceConfig>;
}

export function getApis(): NamedConfig<UserApi.ApiConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getApi(scope));
}

export function getApi(scope: "minimal"): MinimalNamedConfig<UserApi.ApiConfig>;
export function getApi(scope: "full"): FullNamedConfig<UserApi.ApiConfig>;
export function getApi(scope: ConfigType): NamedConfig<UserApi.ApiConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalApi",
      config: {
        fn: getExtImport(scope, "named"),
        httpRoute: getHttpRoute(scope),
      },
    } satisfies MinimalNamedConfig<UserApi.ApiConfig>;
  }

  return {
    name: "FullApi",
    config: {
      fn: getExtImport(scope, "named"),
      httpRoute: getHttpRoute(scope),
      entities: [getEntity("task")],
      auth: true,
      middlewareConfigFn: getExtImport(scope, "named"),
    },
  } satisfies FullNamedConfig<UserApi.ApiConfig>;
}

export function getHttpRoute(
  scope: "minimal",
): MinimalConfig<UserApi.HttpRoute>;
export function getHttpRoute(scope: "full"): FullConfig<UserApi.HttpRoute>;
export function getHttpRoute(scope: ConfigType): Config<UserApi.HttpRoute> {
  if (scope === "minimal") {
    return {
      method: "GET",
      route: "/foo/bar",
    } satisfies MinimalConfig<UserApi.HttpRoute>;
  }

  return {
    method: "GET",
    route: "/foo/bar",
  } satisfies FullConfig<UserApi.HttpRoute>;
}

export function getJobs(): NamedConfig<UserApi.JobConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getJob(scope));
}

export function getJob(scope: "minimal"): MinimalNamedConfig<UserApi.JobConfig>;
export function getJob(scope: "full"): FullNamedConfig<UserApi.JobConfig>;
export function getJob(scope: ConfigType): NamedConfig<UserApi.JobConfig> {
  if (scope === "minimal") {
    return {
      name: "MinimalJob",
      config: {
        executor: "PgBoss",
        perform: getPerform(scope),
      },
    } satisfies MinimalNamedConfig<UserApi.JobConfig>;
  }

  return {
    name: "FullJob",
    config: {
      executor: "PgBoss",
      perform: getPerform(scope),
      entities: [getEntity("task")],
      schedule: getSchedule(scope),
    },
  } satisfies FullNamedConfig<UserApi.JobConfig>;
}

export function getEmailFromField(
  scope: "minimal",
): MinimalConfig<UserApi.EmailFromField>;
export function getEmailFromField(
  scope: "full",
): FullConfig<UserApi.EmailFromField>;
export function getEmailFromField(
  scope: ConfigType,
): Config<UserApi.EmailFromField> {
  if (scope === "minimal") {
    return {
      email: "test@domain.tld",
    } satisfies MinimalConfig<UserApi.EmailFromField>;
  }

  return {
    email: "test@domain.tld",
    name: "ToDo App",
  } satisfies FullConfig<UserApi.EmailFromField>;
}

export function getExtImport(
  scope: "minimal",
  type: AppSpec.ExtImportKind,
): MinimalConfig<UserApi.ExtImport>;
export function getExtImport(
  scope: "full",
  type: AppSpec.ExtImportKind,
): FullConfig<UserApi.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind,
): Config<UserApi.ExtImport> {
  if (type === "default") {
    if (scope === "minimal") {
      return {
        from: "@src/external",
        importDefault: "defaultExport",
      } satisfies MinimalConfig<UserApi.ExtImport>;
    }

    return {
      from: "@src/external",
      importDefault: "defaultExport",
    } satisfies FullConfig<UserApi.ExtImport>;
  }

  if (type === "named") {
    if (scope === "minimal") {
      return {
        from: "@src/external",
        import: "namedExport",
      } satisfies MinimalConfig<UserApi.ExtImport>;
    }

    return {
      from: "@src/external",
      import: "namedExport",
    } satisfies FullConfig<UserApi.ExtImport>;
  }

  throw new Error(`Unhandled scope or type: scope=${scope}, type=${type}`);
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
