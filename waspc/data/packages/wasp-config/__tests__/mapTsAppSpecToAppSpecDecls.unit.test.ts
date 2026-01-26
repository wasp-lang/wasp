import { describe, expect, test } from "vitest";
import { GET_TS_APP_SPEC } from "../src/_private.js";
import * as AppSpec from "../src/appSpec.js";
import {
  makeRefParser,
  mapApi,
  mapApiNamespace,
  mapApp,
  mapAuth,
  mapAuthMethods,
  mapClient,
  mapCrud,
  mapCrudOperationOptions,
  mapCrudOperations,
  mapDb,
  mapEmailAuth,
  mapEmailFromField,
  mapEmailSender,
  mapEmailVerification,
  mapExternalAuth,
  mapExtImport,
  mapHttpRoute,
  mapJob,
  mapOperation,
  mapPage,
  mapPasswordReset,
  mapPerform,
  mapRoute,
  mapSchedule,
  mapServer,
  mapUsernameAndPassword,
  mapWebSocket,
} from "../src/mapTsAppSpecToAppSpecDecls.js";
import { App } from "../src/publicApi/App.js";
import * as TsAppSpec from "../src/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("mapApp", () => {
  test("should map minimal config correctly", () => {
    testMapApp(Fixtures.createApp("minimal").app);
  });

  test("should map full config correctly", () => {
    testMapApp(Fixtures.createApp("full").app);
  });

  function testMapApp(app: App): void {
    const tsAppSpec = app[GET_TS_APP_SPEC]();

    const entities: string[] = [];
    if (tsAppSpec.auth) {
      if (tsAppSpec.auth.userEntity) {
        entities.push(tsAppSpec.auth.userEntity);
      }
      if (tsAppSpec.auth.externalAuthEntity) {
        entities.push(tsAppSpec.auth.externalAuthEntity);
      }
    }
    const routes: string[] = [];
    if (tsAppSpec.auth) {
      if (tsAppSpec.auth.methods.email?.emailVerification.clientRoute) {
        routes.push(tsAppSpec.auth.methods.email.emailVerification.clientRoute);
      }
      if (tsAppSpec.auth.methods.email?.passwordReset.clientRoute) {
        routes.push(tsAppSpec.auth.methods.email.passwordReset.clientRoute);
      }
    }
    const entityRefParser = makeRefParser("Entity", entities);
    const routeRefParser = makeRefParser("Route", routes);

    const result = mapApp(
      tsAppSpec.app.config,
      entityRefParser,
      routeRefParser,
      tsAppSpec.auth,
      tsAppSpec.client,
      tsAppSpec.server,
      tsAppSpec.db,
      tsAppSpec.emailSender,
      tsAppSpec.websocket,
    );

    expect(result).toStrictEqual({
      wasp: {
        version: tsAppSpec.app.config.wasp.version,
      },
      title: tsAppSpec.app.config.title,
      head: tsAppSpec.app.config.head,
      auth:
        tsAppSpec.auth &&
        mapAuth(tsAppSpec.auth, entityRefParser, routeRefParser),
      server: tsAppSpec.server && mapServer(tsAppSpec.server),
      client: tsAppSpec.client && mapClient(tsAppSpec.client),
      db: tsAppSpec.db && mapDb(tsAppSpec.db),
      emailSender:
        tsAppSpec.emailSender && mapEmailSender(tsAppSpec.emailSender),
      webSocket: tsAppSpec.websocket && mapWebSocket(tsAppSpec.websocket),
    } satisfies AppSpec.App);
  }
});

describe("mapAuth", () => {
  test("should map minimal config correctly", () => {
    testMapAuth(Fixtures.getAuthConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapAuth(Fixtures.getAuthConfig("full"));
  });

  test("should throw if userEntity is not provided to entity parser", () => {
    testMapAuth(Fixtures.getAuthConfig("minimal"), {
      overrideEntities: [],
      shouldError: true,
    });
  });

  test("should throw if externalAuthEntity ref is not provided when defined", () => {
    const auth = Fixtures.getAuthConfig("full");
    expect(auth.externalAuthEntity).toBeDefined();
    testMapAuth(auth, {
      overrideEntities: [auth.userEntity],
      shouldError: true,
    });
  });

  test("should throw if emailVerification clientRoute ref is not provided when defined", () => {
    const auth = Fixtures.getAuthConfig("full");
    expect(auth.methods.email.emailVerification.clientRoute).toBeDefined();
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const auth = Fixtures.getAuthConfig("full");
    expect(auth.methods.email.passwordReset.clientRoute).toBeDefined();
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.emailVerification.clientRoute],
      shouldError: true,
    });
  });

  function testMapAuth(
    auth: TsAppSpec.AuthConfig,
    options:
      | {
          overrideEntities?: string[];
          overrideRoutes?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideEntities, overrideRoutes, shouldError } = options;
    const entities =
      overrideEntities ??
      [auth.userEntity, auth.externalAuthEntity].filter((e) => e !== undefined);
    const routes =
      overrideRoutes ??
      [
        auth.methods.email?.emailVerification.clientRoute,
        auth.methods.email?.passwordReset.clientRoute,
      ].filter((e) => e !== undefined);
    const entityRefParser = makeRefParser("Entity", entities);
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() =>
        mapAuth(auth, entityRefParser, routeRefParser),
      ).toThrowError();
      return;
    }

    const result = mapAuth(auth, entityRefParser, routeRefParser);

    expect(result).toStrictEqual({
      userEntity: entityRefParser(auth.userEntity),
      externalAuthEntity:
        auth.externalAuthEntity === undefined
          ? undefined
          : entityRefParser(auth.externalAuthEntity),
      methods: mapAuthMethods(auth.methods, routeRefParser),
      onAuthFailedRedirectTo: auth.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: auth.onAuthSucceededRedirectTo,
      onBeforeSignup: auth.onBeforeSignup && mapExtImport(auth.onBeforeSignup),
      onAfterSignup: auth.onAfterSignup && mapExtImport(auth.onAfterSignup),
      onAfterEmailVerified:
        auth.onAfterEmailVerified && mapExtImport(auth.onAfterEmailVerified),
      onBeforeOAuthRedirect:
        auth.onBeforeOAuthRedirect && mapExtImport(auth.onBeforeOAuthRedirect),
      onBeforeLogin: auth.onBeforeLogin && mapExtImport(auth.onBeforeLogin),
      onAfterLogin: auth.onAfterLogin && mapExtImport(auth.onAfterLogin),
    } satisfies AppSpec.Auth);
  }
});

describe("mapAuthMethods", () => {
  test("should map minimal config correctly", () => {
    testMapAuthMethods(Fixtures.getAuthMethods("minimal"));
  });

  test("should map full config correctly", () => {
    testMapAuthMethods(Fixtures.getAuthMethods("full"));
  });

  test("should throw if emailVerification clientRoute ref is not provided when defined", () => {
    const authMethods = Fixtures.getAuthMethods("full");
    expect(authMethods.email.emailVerification.clientRoute).toBeDefined();
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.emailVerification.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const authMethods = Fixtures.getAuthMethods("full");
    expect(authMethods.email.passwordReset.clientRoute).toBeDefined();
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  function testMapAuthMethods(
    authMethods: TsAppSpec.AuthMethods,
    options:
      | {
          overrideRoutes?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideRoutes, shouldError } = options;
    const routes =
      overrideRoutes ??
      [
        authMethods.email?.emailVerification.clientRoute,
        authMethods.email?.passwordReset.clientRoute,
      ].filter((e) => e !== undefined);
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() => mapAuthMethods(authMethods, routeRefParser)).toThrowError();
      return;
    }

    const result = mapAuthMethods(authMethods, routeRefParser);

    expect(result).toStrictEqual({
      usernameAndPassword:
        authMethods.usernameAndPassword &&
        mapUsernameAndPassword(authMethods.usernameAndPassword),
      discord: authMethods.discord && mapExternalAuth(authMethods.discord),
      google: authMethods.google && mapExternalAuth(authMethods.google),
      gitHub: authMethods.gitHub && mapExternalAuth(authMethods.gitHub),
      keycloak: authMethods.keycloak && mapExternalAuth(authMethods.keycloak),
      email:
        authMethods.email && mapEmailAuth(authMethods.email, routeRefParser),
    } satisfies AppSpec.AuthMethods);
  }
});

describe("mapEmailAuth", () => {
  test("should map minimal config correctly", () => {
    testMapEmailAuth(Fixtures.getEmailAuthConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailAuth(Fixtures.getEmailAuthConfig("full"));
  });

  test("should throw if emailVerification clientRoute ref is not provided when defined", () => {
    const emailAuth = Fixtures.getEmailAuthConfig("full");
    expect(emailAuth.emailVerification.clientRoute).toBeDefined();
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const emailAuth = Fixtures.getEmailAuthConfig("full");
    expect(emailAuth.passwordReset.clientRoute).toBeDefined();
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.emailVerification.clientRoute],
      shouldError: true,
    });
  });

  function testMapEmailAuth(
    emailAuth: TsAppSpec.EmailAuthConfig,
    options:
      | {
          overrideRoutes?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideRoutes, shouldError } = options;
    const routes =
      overrideRoutes ??
      [
        emailAuth?.emailVerification.clientRoute,
        emailAuth?.passwordReset.clientRoute,
      ].filter((e) => e !== undefined);
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() => mapEmailAuth(emailAuth, routeRefParser)).toThrowError();
      return;
    }

    const result = mapEmailAuth(emailAuth, routeRefParser);

    expect(result).toStrictEqual({
      userSignupFields:
        emailAuth.userSignupFields && mapExtImport(emailAuth.userSignupFields),
      fromField: mapEmailFromField(emailAuth.fromField),
      emailVerification: mapEmailVerification(
        emailAuth.emailVerification,
        routeRefParser,
      ),
      passwordReset: mapPasswordReset(emailAuth.passwordReset, routeRefParser),
    } satisfies AppSpec.EmailAuthConfig);
  }
});

describe("mapEmailVerification", () => {
  test("should map minimal config correctly", () => {
    testMapEmailVerification(Fixtures.getEmailVerificationConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailVerification(Fixtures.getEmailVerificationConfig("full"));
  });

  test("should throw if clientRoute ref is not provided when defined", () => {
    const emailVerification = Fixtures.getEmailVerificationConfig("full");
    expect(emailVerification.clientRoute).toBeDefined();
    testMapEmailVerification(emailVerification, {
      overrideRoutes: [],
      shouldError: true,
    });
  });

  function testMapEmailVerification(
    emailVerification: TsAppSpec.EmailVerificationConfig,
    options:
      | {
          overrideRoutes?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideRoutes, shouldError } = options;
    const routes =
      overrideRoutes ??
      [emailVerification.clientRoute].filter((e) => e !== undefined);
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() =>
        mapEmailVerification(emailVerification, routeRefParser),
      ).toThrowError();
      return;
    }

    const result = mapEmailVerification(emailVerification, routeRefParser);

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(emailVerification.clientRoute),
      getEmailContentFn:
        emailVerification.getEmailContentFn &&
        mapExtImport(emailVerification.getEmailContentFn),
    } satisfies AppSpec.EmailVerificationConfig);
  }
});

describe("mapPasswordReset", () => {
  test("should map minimal config correctly", () => {
    testMapPasswordReset(Fixtures.getPasswordResetConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapPasswordReset(Fixtures.getPasswordResetConfig("full"));
  });

  test("should throw if clientRoute ref is not provided when defined", () => {
    const passwordReset = Fixtures.getPasswordResetConfig("full");
    expect(passwordReset.clientRoute).toBeDefined();
    testMapPasswordReset(passwordReset, {
      overrideRoutes: [],
      shouldError: true,
    });
  });

  function testMapPasswordReset(
    passwordReset: TsAppSpec.PasswordResetConfig,
    options:
      | {
          overrideRoutes?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideRoutes, shouldError } = options;
    const routes =
      overrideRoutes ??
      [passwordReset.clientRoute].filter((e) => e !== undefined);
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() =>
        mapPasswordReset(passwordReset, routeRefParser),
      ).toThrowError();
      return;
    }

    const result = mapPasswordReset(passwordReset, routeRefParser);

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(passwordReset.clientRoute),
      getEmailContentFn:
        passwordReset.getEmailContentFn &&
        mapExtImport(passwordReset.getEmailContentFn),
    } satisfies AppSpec.PasswordResetConfig);
  }
});

describe("mapUsernameAndPassword", () => {
  test("should map minimal config correctly", () => {
    testMapUsernameAndPassword(
      Fixtures.getUsernameAndPasswordConfig("minimal"),
    );
  });

  test("should map full config correctly", () => {
    testMapUsernameAndPassword(Fixtures.getUsernameAndPasswordConfig("full"));
  });

  function testMapUsernameAndPassword(
    usernameAndPassword: TsAppSpec.UsernameAndPasswordConfig,
  ): void {
    const result = mapUsernameAndPassword(usernameAndPassword);

    expect(result).toStrictEqual({
      userSignupFields:
        usernameAndPassword.userSignupFields &&
        mapExtImport(usernameAndPassword.userSignupFields),
    } satisfies AppSpec.UsernameAndPasswordConfig);
  }
});

describe("mapExternalAuth", () => {
  test("should map minimal config correctly", () => {
    testMapExternalAuth(Fixtures.getExternalAuthConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapExternalAuth(Fixtures.getExternalAuthConfig("full"));
  });

  function testMapExternalAuth(
    externalAuth: TsAppSpec.ExternalAuthConfig,
  ): void {
    const result = mapExternalAuth(externalAuth);

    expect(result).toStrictEqual({
      configFn: externalAuth.configFn && mapExtImport(externalAuth.configFn),
      userSignupFields:
        externalAuth.userSignupFields &&
        mapExtImport(externalAuth.userSignupFields),
    } satisfies AppSpec.ExternalAuthConfig);
  }
});

describe("mapClient", () => {
  test("should map minimal config correctly", () => {
    testMapClient(Fixtures.getClientConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapClient(Fixtures.getClientConfig("full"));
  });

  function testMapClient(client: TsAppSpec.ClientConfig): void {
    const result = mapClient(client);

    expect(result).toStrictEqual({
      rootComponent: client.rootComponent && mapExtImport(client.rootComponent),
      setupFn: client.setupFn && mapExtImport(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema:
        client.envValidationSchema && mapExtImport(client.envValidationSchema),
    } satisfies AppSpec.Client);
  }
});

describe("mapServer", () => {
  test("should map minimal config correctly", () => {
    testMapServer(Fixtures.getServerConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapServer(Fixtures.getServerConfig("full"));
  });

  function testMapServer(server: TsAppSpec.ServerConfig): void {
    const result = mapServer(server);

    expect(result).toStrictEqual({
      setupFn: server.setupFn && mapExtImport(server.setupFn),
      middlewareConfigFn:
        server.middlewareConfigFn && mapExtImport(server.middlewareConfigFn),
      envValidationSchema:
        server.envValidationSchema && mapExtImport(server.envValidationSchema),
    } satisfies AppSpec.Server);
  }
});

describe("mapEmailSender", () => {
  test("should map minimal config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("full"));
  });

  function testMapEmailSender(emailSender: TsAppSpec.EmailSenderConfig): void {
    const result = mapEmailSender(emailSender);

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom:
        emailSender.defaultFrom && mapEmailFromField(emailSender.defaultFrom),
    } satisfies AppSpec.EmailSender);
  }
});

describe("mapWebSocket", () => {
  test("should map minimal config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("full"));
  });

  function testMapWebSocket(websocket: TsAppSpec.WebsocketConfig): void {
    const result = mapWebSocket(websocket);

    expect(result).toStrictEqual({
      fn: mapExtImport(websocket.fn),
      autoConnect: websocket.autoConnect,
    } satisfies AppSpec.WebSocket);
  }
});

describe("mapDb", () => {
  test("should map minimal config correctly", () => {
    testDb(Fixtures.getDbConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testDb(Fixtures.getDbConfig("full"));
  });

  function testDb(db: TsAppSpec.DbConfig): void {
    const result = mapDb(db);

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapExtImport(seed)),
      prismaSetupFn: db.prismaSetupFn && mapExtImport(db.prismaSetupFn),
    } satisfies AppSpec.Db);
  }
});

describe("mapPage", () => {
  test("should map minimal config correctly", () => {
    testMapPage(Fixtures.getPageConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapPage(Fixtures.getPageConfig("full").config);
  });

  function testMapPage(page: TsAppSpec.PageConfig): void {
    const result = mapPage(page);

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: page.authRequired,
    } satisfies AppSpec.Page);
  }
});

describe("mapRoute", () => {
  // NOTE: currently minimal config is the same as full config
  test("should map minimal config correctly", () => {
    testMapRoute(Fixtures.getRouteConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapRoute(Fixtures.getRouteConfig("full").config);
  });

  function testMapRoute(route: TsAppSpec.RouteConfig): void {
    const pageRefParser = makeRefParser("Page", [route.to]);

    const result = mapRoute(route, pageRefParser);

    expect(result).toStrictEqual({
      path: route.path,
      to: pageRefParser(route.to),
    } satisfies AppSpec.Route);
  }
});

describe("mapOperation", () => {
  test("should map minimal query config correctly", () => {
    testMapOperation(Fixtures.getQueryConfig("minimal").config);
  });

  test("should map full query config correctly", () => {
    testMapOperation(Fixtures.getQueryConfig("full").config);
  });

  test("should throw if entity ref is not provided in query config", () => {
    testMapOperation(Fixtures.getQueryConfig("full").config, {
      overrideEntities: [],
      shouldError: true,
    });
  });

  test("should map minimal action config correctly", () => {
    testMapOperation(Fixtures.getActionConfig("minimal").config);
  });

  test("should map action config correctly", () => {
    testMapOperation(Fixtures.getActionConfig("full").config);
  });

  test("should throw if entity ref is not provided in action config", () => {
    testMapOperation(Fixtures.getActionConfig("full").config, {
      overrideEntities: [],
      shouldError: true,
    });
  });

  function testMapOperation(
    operation: TsAppSpec.ActionConfig | TsAppSpec.QueryConfig,
    options:
      | {
          overrideEntities?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideEntities, shouldError } = options;
    const entities =
      overrideEntities ??
      operation.entities?.filter((e) => e !== undefined) ??
      [];
    const entityRefParser = makeRefParser("Entity", entities);

    if (shouldError) {
      expect(() => mapOperation(operation, entityRefParser)).toThrowError();
      return;
    }

    const result = mapOperation(operation, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(operation.fn),
      entities: operation.entities?.map(entityRefParser),
      auth: operation.auth,
    } satisfies AppSpec.Query);
  }
});

describe("mapCrud", () => {
  test("should map minimal config correctly", () => {
    testMapCrud(Fixtures.getCrudConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapCrud(Fixtures.getCrudConfig("full").config);
  });

  test("should throw if entity ref is not provided", () => {
    testMapCrud(Fixtures.getCrudConfig("full").config, {
      overrideEntities: [],
      shouldError: true,
    });
  });

  function testMapCrud(
    crud: TsAppSpec.CrudConfig,
    options:
      | {
          overrideEntities?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideEntities, shouldError } = options;
    const entities =
      overrideEntities ?? [crud.entity].filter((e) => e !== undefined);
    const entityRefParser = makeRefParser("Entity", entities);

    if (shouldError) {
      expect(() => mapCrud(crud, entityRefParser)).toThrowError();
      return;
    }

    const result = mapCrud(crud, entityRefParser);

    expect(result).toStrictEqual({
      entity: entityRefParser(crud.entity),
      operations: mapCrudOperations(crud.operations),
    } satisfies AppSpec.Crud);
  }
});

describe("mapCrudOperations", () => {
  test("should map minimal config correctly", () => {
    testMapCrudOperations(Fixtures.getCrudOperations("minimal"));
  });

  test("should map full config correctly", () => {
    testMapCrudOperations(Fixtures.getCrudOperations("full"));
  });

  function testMapCrudOperations(
    crudOperations: TsAppSpec.CrudOperations,
  ): void {
    const result = mapCrudOperations(crudOperations);

    expect(result).toStrictEqual({
      get: crudOperations.get && mapCrudOperationOptions(crudOperations.get),
      getAll:
        crudOperations.getAll && mapCrudOperationOptions(crudOperations.getAll),
      create:
        crudOperations.create && mapCrudOperationOptions(crudOperations.create),
      update:
        crudOperations.update && mapCrudOperationOptions(crudOperations.update),
      delete:
        crudOperations.delete && mapCrudOperationOptions(crudOperations.delete),
    } satisfies AppSpec.CrudOperations);
  }
});

describe("mapCrudOperationOptions", () => {
  test("should map minimal config correctly", () => {
    testMapCrudOperationOptions(Fixtures.getCrudOperationOptions("minimal"));
  });

  test("should map full config correctly", () => {
    testMapCrudOperationOptions(Fixtures.getCrudOperationOptions("full"));
  });

  function testMapCrudOperationOptions(
    crudOperationOptions: TsAppSpec.CrudOperationOptions,
  ): void {
    const result = mapCrudOperationOptions(crudOperationOptions);

    expect(result).toStrictEqual({
      isPublic: crudOperationOptions.isPublic,
      overrideFn:
        crudOperationOptions.overrideFn &&
        mapExtImport(crudOperationOptions.overrideFn),
    } satisfies AppSpec.CrudOperationOptions);
  }
});

describe("mapApiNamespace", () => {
  // NOTE: currently minimal config is the same as full config
  test("should map minimal config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespaceConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespaceConfig("full").config);
  });

  function testMapApiNamespace(
    apiNamespace: TsAppSpec.ApiNamespaceConfig,
  ): void {
    const result = mapApiNamespace(apiNamespace);

    expect(result).toStrictEqual({
      middlewareConfigFn: mapExtImport(apiNamespace.middlewareConfigFn),
      path: apiNamespace.path,
    } satisfies AppSpec.ApiNamespace);
  }
});

describe("mapApi", () => {
  test("should map minimal config correctly", () => {
    testMapApi(Fixtures.getApiConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapApi(Fixtures.getApiConfig("full").config);
  });

  test("should throw if entities refs are not provided", () => {
    testMapApi(Fixtures.getApiConfig("full").config, {
      overrideEntities: [],
      shouldError: true,
    });
  });

  function testMapApi(
    api: TsAppSpec.ApiConfig,
    options:
      | {
          overrideEntities?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideEntities, shouldError } = options;
    const entities =
      overrideEntities ?? api.entities?.filter((e) => e !== undefined) ?? [];
    const entityRefParser = makeRefParser("Entity", entities);

    if (shouldError) {
      expect(() => mapApi(api, entityRefParser)).toThrowError();
      return;
    }

    const result = mapApi(api, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(api.fn),
      middlewareConfigFn:
        api.middlewareConfigFn && mapExtImport(api.middlewareConfigFn),
      entities: api.entities?.map(entityRefParser),
      httpRoute: mapHttpRoute(api.httpRoute),
      auth: api.auth,
    } satisfies AppSpec.Api);
  }
});

describe("mapHttpRoute", () => {
  test("should map minimal config correctly", () => {
    testMapHttpRoute(Fixtures.getHttpRoute("minimal"));
  });

  test("should map full config correctly", () => {
    testMapHttpRoute(Fixtures.getHttpRoute("full"));
  });

  function testMapHttpRoute(httpRoute: TsAppSpec.HttpRoute): void {
    const result = mapHttpRoute(httpRoute);

    expect(result).toStrictEqual([
      httpRoute.method,
      httpRoute.route,
    ] satisfies AppSpec.HttpRoute);
  }
});

describe("mapJob", () => {
  test("should map minimal config correctly", () => {
    testMapJob(Fixtures.getJobConfig("minimal").config);
  });

  test("should map full config correctly", () => {
    testMapJob(Fixtures.getJobConfig("full").config);
  });

  test("should throw if entity ref is not provided", () => {
    testMapJob(Fixtures.getJobConfig("full").config, {
      overrideEntities: [],
      shouldError: true,
    });
  });

  function testMapJob(
    job: TsAppSpec.JobConfig,
    options:
      | {
          overrideEntities?: string[];
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { overrideEntities, shouldError } = options;
    const entities =
      overrideEntities ?? job.entities?.filter((e) => e !== undefined) ?? [];
    const entityRefParser = makeRefParser("Entity", entities);

    if (shouldError) {
      expect(() => mapJob(job, entityRefParser)).toThrowError();
      return;
    }

    const result = mapJob(job, entityRefParser);

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: mapPerform(job.perform),
      schedule: job.schedule && mapSchedule(job.schedule),
      entities: job.entities?.map(entityRefParser),
    } satisfies AppSpec.Job);
  }
});

describe("mapSchedule", () => {
  test("should map minimal config correctly", () => {
    testMapSchedule(Fixtures.getSchedule("minimal"));
  });

  test("should map full config correctly", () => {
    testMapSchedule(Fixtures.getSchedule("full"));
  });

  function testMapSchedule(schedule: TsAppSpec.Schedule): void {
    const result = mapSchedule(schedule);

    expect(result).toStrictEqual({
      cron: schedule.cron,
      args: schedule.args,
      executorOptions: schedule.executorOptions,
    } satisfies AppSpec.Schedule);
  }
});

describe("mapPerform", () => {
  test("should map minimal config correctly", () => {
    testMapPerform(Fixtures.getPerform("minimal"));
  });

  test("should map full config correctly", () => {
    testMapPerform(Fixtures.getPerform("full"));
  });

  function testMapPerform(perform: TsAppSpec.Perform): void {
    const result = mapPerform(perform);

    expect(result).toStrictEqual({
      fn: mapExtImport(perform.fn),
      executorOptions: perform.executorOptions,
    } satisfies AppSpec.Perform);
  }
});

describe("mapEmailFromField", () => {
  test("should map minimal config correctly", () => {
    testMapEmailFromField(Fixtures.getEmailFromField("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailFromField(Fixtures.getEmailFromField("full"));
  });

  function testMapEmailFromField(
    emailFromField: TsAppSpec.EmailFromField,
  ): void {
    const result = mapEmailFromField(emailFromField);

    expect(result).toStrictEqual({
      name: emailFromField.name,
      email: emailFromField.email,
    } satisfies AppSpec.EmailFromField);
  }
});

describe("mapExtImport", () => {
  test("should map minimal named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "named"));
  });

  test("should map full named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "named"));
  });

  test("should map minimal default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "default"));
  });

  test("should map full default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "default"));
  });

  test("should throw for missing import kind", () => {
    const extImport: TsAppSpec.ExtImport = {
      from: "@src/myModule",
    } as unknown as TsAppSpec.ExtImport;

    testMapExtImport(extImport, {
      shouldError: true,
    });
  });

  // TODO: unskip this test when we decide how to handle this
  test.skip("should throw for having both import kind", () => {
    const extImport = {
      ...Fixtures.getExtImport("full", "named"),
      ...Fixtures.getExtImport("full", "default"),
    };

    testMapExtImport(extImport, {
      shouldError: true,
    });
  });

  // TODO: unskip this test when we decide how to handle this
  test.skip("should throw error for invalid from path", () => {
    const extImport: TsAppSpec.ExtImport = {
      import: "myNamedImport",
      from: "./invalid/path",
    } as unknown as TsAppSpec.ExtImport;

    testMapExtImport(extImport, {
      shouldError: true,
    });
  });

  function testMapExtImport(
    extImport: TsAppSpec.ExtImport,
    options:
      | {
          shouldError: boolean | undefined;
        }
      | undefined = {
      shouldError: false,
    },
  ): void {
    const { shouldError } = options;

    if (shouldError) {
      expect(() => mapExtImport(extImport)).toThrowError();
      return;
    }

    const result = mapExtImport(extImport);

    if ("import" in extImport) {
      expect(result).toStrictEqual({
        kind: "named",
        name: extImport.import,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    } else if ("importDefault" in extImport) {
      expect(result).toStrictEqual({
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    }
  }
});
