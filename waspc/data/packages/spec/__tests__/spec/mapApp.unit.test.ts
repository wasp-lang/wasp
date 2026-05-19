import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { mapExtImport } from "../../src/spec/extImport.js";
import {
  deriveExtImportName,
  makeRefParser,
  mapAction,
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
  mapEmailFlow,
  mapEmailFromField,
  mapEmailSender,
  mapExternalAuth,
  mapJob,
  mapPage,
  mapQuery,
  mapRoute,
  mapSchedule,
  mapServer,
  mapUsernameAndPassword,
  mapWebSocket,
} from "../../src/spec/mapApp.js";
import { app, page, route } from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("mapApp", () => {
  test("should map minimal app correctly", () => {
    const entityNames = Fixtures.getEntities("minimal");
    const app = Fixtures.getApp("minimal");

    const decls = mapApp(app, entityNames);

    expect(decls).toStrictEqual([
      {
        declType: "App",
        declName: app.name,
        declValue: {
          wasp: app.wasp,
          title: app.title,
          head: app.head,
          auth: undefined,
          server: undefined,
          client: undefined,
          db: undefined,
          emailSender: undefined,
          webSocket: undefined,
        },
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("should map full app correctly", () => {
    // Note: we build the full app inline (instead of using `Fixtures.getApp("full")`)
    // so the assertions can name each part explicitly when computing expected decls.
    const page = Fixtures.getPage("full");
    const route = Fixtures.getRoute("full");
    const query = Fixtures.getQuery("full");
    const api = Fixtures.getApi("full");
    const apiNamespace = Fixtures.getApiNamespace("full");
    const job = Fixtures.getJob("full");
    const crud = Fixtures.getCrud("full");
    const emailVerifyRoute = Fixtures.getEmailVerifyRoute();
    const passwordResetRoute = Fixtures.getPasswordResetRoute();
    const authConfig = Fixtures.getAuthConfig("full");
    const server = Fixtures.getServerConfig("full");
    const client = Fixtures.getClientConfig("full");
    const db = Fixtures.getDbConfig("full");
    const emailSender = Fixtures.getEmailSenderConfig("full");
    const webSocket = Fixtures.getWebSocketConfig("full");
    const entityNames = Fixtures.getEntities("full");
    const entityRefParser = makeRefParser("Entity", entityNames);

    const inputApp = app({
      name: "FullApp",
      wasp: { version: "^0.16.3" },
      title: "Mock App",
      head: ['<link rel="icon" href="/favicon.ico" />'],
      auth: authConfig,
      server,
      client,
      db,
      emailSender,
      webSocket,
      parts: [
        page,
        route,
        query,
        api,
        apiNamespace,
        job,
        crud,
        emailVerifyRoute,
        passwordResetRoute,
      ],
    });

    const result = mapApp(inputApp, entityNames);

    const routeRefParser = makeRefParser("Route", [
      emailVerifyRoute.name,
      passwordResetRoute.name,
    ]);

    // TODO: Reaching into `deriveExtImportName` here is not ideal — it leaks
    // an orchestrator-internal helper into the test. Revisit once we have a
    // higher-level name-derivation system: either a part-agnostic
    // `deriveDeclName(part)`, or a part-specific dispatch (mirroring how we
    // have one mapper per kind). The test should delegate to whatever
    // surfaces.
    expect(result).toStrictEqual([
      {
        declType: "App",
        declName: inputApp.name,
        declValue: {
          wasp: inputApp.wasp,
          title: inputApp.title,
          head: inputApp.head,
          auth: mapAuth(authConfig, entityRefParser, routeRefParser),
          server: mapServer(server),
          client: mapClient(client),
          db: mapDb(db),
          emailSender: mapEmailSender(emailSender),
          webSocket: mapWebSocket(webSocket),
        },
      },
      {
        declType: "Page",
        declName: deriveExtImportName(page.component),
        declValue: mapPage(page),
      },
      {
        declType: "Page",
        declName: deriveExtImportName(emailVerifyRoute.page.component),
        declValue: mapPage(emailVerifyRoute.page),
      },
      {
        declType: "Page",
        declName: deriveExtImportName(passwordResetRoute.page.component),
        declValue: mapPage(passwordResetRoute.page),
      },
      {
        declType: "Route",
        declName: route.name,
        declValue: mapRoute(route),
      },
      {
        declType: "Route",
        declName: emailVerifyRoute.name,
        declValue: mapRoute(emailVerifyRoute),
      },
      {
        declType: "Route",
        declName: passwordResetRoute.name,
        declValue: mapRoute(passwordResetRoute),
      },
      {
        declType: "Query",
        declName: deriveExtImportName(query.fn),
        declValue: mapQuery(query, entityRefParser),
      },
      {
        declType: "Api",
        declName: deriveExtImportName(api.fn),
        declValue: mapApi(api, entityRefParser),
      },
      {
        declType: "ApiNamespace",
        declName: deriveExtImportName(apiNamespace.middlewareConfigFn),
        declValue: mapApiNamespace(apiNamespace),
      },
      {
        declType: "Job",
        declName: deriveExtImportName(job.fn),
        declValue: mapJob(job, entityRefParser),
      },
      {
        declType: "Crud",
        declName: crud.name,
        declValue: mapCrud(crud, entityRefParser),
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("dedups a page referenced explicitly twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);

    const app = Fixtures.getMinimalAppWithParts([page1, page2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced via a route shorthand twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([route1, route2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([page1, route1]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("throws when the same page name is produced with differing configs explicitly", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });

    const app = Fixtures.getMinimalAppWithParts([page1, page2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs via a route shorthand twice", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([route1, route2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getExtImport("minimal", "default");
    const pageName = deriveExtImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithParts([page1, route1]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  // TODO: duplicate query name → throw.
});

describe("mapPage", () => {
  test("should map minimal config correctly", () => {
    testMapPage(Fixtures.getPage("minimal"));
  });

  test("should map full config correctly", () => {
    testMapPage(Fixtures.getPage("full"));
  });

  function testMapPage(page: TsAppSpec.Page): void {
    const result = mapPage(page);

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: page.authRequired,
    } satisfies AppSpec.Page);
  }
});

describe("mapRoute", () => {
  test("should map minimal config correctly", () => {
    testMapRoute(Fixtures.getRoute("minimal"));
  });

  test("should map full config correctly", () => {
    testMapRoute(Fixtures.getRoute("full"));
  });

  function testMapRoute(route: TsAppSpec.Route): void {
    const result = mapRoute(route);

    expect(result).toStrictEqual({
      path: route.path,
      to: {
        name: deriveExtImportName(route.page.component),
        declType: "Page",
      },
      prerender: route.prerender,
      lazy: route.lazy,
    } satisfies AppSpec.Route);
  }
});

describe("mapQuery", () => {
  test("should map minimal config correctly", () => {
    testMapQuery(Fixtures.getQuery("minimal"));
  });

  test("should map full config correctly", () => {
    testMapQuery(Fixtures.getQuery("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const query = Fixtures.getQuery("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapQuery(query, entityRefParser)).toThrowError();
  });

  function testMapQuery(query: TsAppSpec.Query): void {
    const entityRefParser = makeRefParser("Entity", query.entities ?? []);

    const result = mapQuery(query, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(query.fn),
      entities: query.entities?.map(entityRefParser),
      auth: query.auth,
    } satisfies AppSpec.Query);
  }
});

describe("mapAction", () => {
  test("should map minimal config correctly", () => {
    testMapAction(Fixtures.getAction("minimal"));
  });

  test("should map full config correctly", () => {
    testMapAction(Fixtures.getAction("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const action = Fixtures.getAction("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapAction(action, entityRefParser)).toThrowError();
  });

  function testMapAction(action: TsAppSpec.Action): void {
    const entityRefParser = makeRefParser("Entity", action.entities ?? []);

    const result = mapAction(action, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(action.fn),
      entities: action.entities?.map(entityRefParser),
      auth: action.auth,
    } satisfies AppSpec.Action);
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
    assertDefined(auth.methods.email?.emailVerification.clientRoute);
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const auth = Fixtures.getAuthConfig("full");
    assertDefined(auth.methods.email?.passwordReset.clientRoute);
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.emailVerification.clientRoute],
      shouldError: true,
    });
  });

  function testMapAuth(
    auth: TsAppSpec.Auth,
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
    assertDefined(authMethods.email?.emailVerification.clientRoute);
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const authMethods = Fixtures.getAuthMethods("full");
    assertDefined(authMethods.email?.passwordReset.clientRoute);
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.emailVerification.clientRoute],
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
      microsoft:
        authMethods.microsoft && mapExternalAuth(authMethods.microsoft),
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
      emailVerification: mapEmailFlow(
        emailAuth.emailVerification,
        routeRefParser,
      ),
      passwordReset: mapEmailFlow(emailAuth.passwordReset, routeRefParser),
    } satisfies AppSpec.EmailAuthConfig);
  }
});

describe("mapEmailFlow", () => {
  test("should map minimal email verification config correctly", () => {
    testMapEmailFlow(Fixtures.getEmailVerificationConfig("minimal"));
  });

  test("should map full email verification config correctly", () => {
    testMapEmailFlow(Fixtures.getEmailVerificationConfig("full"));
  });

  test("should map minimal password reset config correctly", () => {
    testMapEmailFlow(Fixtures.getPasswordResetConfig("minimal"));
  });

  test("should map full password reset config correctly", () => {
    testMapEmailFlow(Fixtures.getPasswordResetConfig("full"));
  });

  test("should throw if clientRoute ref is not provided when defined", () => {
    const emailFlow = Fixtures.getEmailVerificationConfig("full");
    expect(emailFlow.clientRoute).toBeDefined();
    testMapEmailFlow(emailFlow, {
      overrideRoutes: [],
      shouldError: true,
    });
  });

  function testMapEmailFlow(
    emailFlow: TsAppSpec.EmailFlowConfig,
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
    const routes = overrideRoutes ?? [emailFlow.clientRoute];
    const routeRefParser = makeRefParser("Route", routes);

    if (shouldError) {
      expect(() => mapEmailFlow(emailFlow, routeRefParser)).toThrowError();
      return;
    }

    const result = mapEmailFlow(emailFlow, routeRefParser);

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(emailFlow.clientRoute),
      getEmailContentFn:
        emailFlow.getEmailContentFn &&
        mapExtImport(emailFlow.getEmailContentFn),
    } satisfies AppSpec.EmailVerificationConfig);
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

describe("mapApi", () => {
  test("should map minimal config correctly", () => {
    testMapApi(Fixtures.getApi("minimal"));
  });

  test("should map full config correctly", () => {
    testMapApi(Fixtures.getApi("full"));
  });

  test("should throw if entity refs are not provided", () => {
    const api = Fixtures.getApi("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapApi(api, entityRefParser)).toThrowError();
  });

  function testMapApi(api: TsAppSpec.Api): void {
    const entityRefParser = makeRefParser("Entity", api.entities ?? []);

    const result = mapApi(api, entityRefParser);

    expect(result).toStrictEqual({
      fn: mapExtImport(api.fn),
      middlewareConfigFn:
        api.middlewareConfigFn && mapExtImport(api.middlewareConfigFn),
      entities: api.entities?.map(entityRefParser),
      httpRoute: [api.method, api.path],
      auth: api.auth,
    } satisfies AppSpec.Api);
  }
});

describe("mapApiNamespace", () => {
  test("should map minimal config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespace("minimal"));
  });

  test("should map full config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespace("full"));
  });

  function testMapApiNamespace(apiNamespace: TsAppSpec.ApiNamespace): void {
    const result = mapApiNamespace(apiNamespace);

    expect(result).toStrictEqual({
      middlewareConfigFn: mapExtImport(apiNamespace.middlewareConfigFn),
      path: apiNamespace.path,
    } satisfies AppSpec.ApiNamespace);
  }
});

describe("mapServer", () => {
  test("should map minimal config correctly", () => {
    testMapServer(Fixtures.getServerConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapServer(Fixtures.getServerConfig("full"));
  });

  function testMapServer(server: TsAppSpec.Server): void {
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

describe("mapClient", () => {
  test("should map minimal config correctly", () => {
    testMapClient(Fixtures.getClientConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapClient(Fixtures.getClientConfig("full"));
  });

  function testMapClient(client: TsAppSpec.Client): void {
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

describe("mapDb", () => {
  test("should map minimal config correctly", () => {
    testMapDb(Fixtures.getDbConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapDb(Fixtures.getDbConfig("full"));
  });

  function testMapDb(db: TsAppSpec.Db): void {
    const result = mapDb(db);

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapExtImport(seed)),
      prismaSetupFn: db.prismaSetupFn && mapExtImport(db.prismaSetupFn),
    } satisfies AppSpec.Db);
  }
});

describe("mapEmailSender", () => {
  test("should map minimal config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapEmailSender(Fixtures.getEmailSenderConfig("full"));
  });

  function testMapEmailSender(emailSender: TsAppSpec.EmailSender): void {
    const result = mapEmailSender(emailSender);

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom:
        emailSender.defaultFrom && mapEmailFromField(emailSender.defaultFrom),
    } satisfies AppSpec.EmailSender);
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

describe("mapWebSocket", () => {
  test("should map minimal config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapWebSocket(Fixtures.getWebSocketConfig("full"));
  });

  function testMapWebSocket(webSocket: TsAppSpec.WebSocket): void {
    const result = mapWebSocket(webSocket);

    expect(result).toStrictEqual({
      fn: mapExtImport(webSocket.fn),
      autoConnect: webSocket.autoConnect,
    } satisfies AppSpec.WebSocket);
  }
});

describe("mapJob", () => {
  test("should map minimal config correctly", () => {
    testMapJob(Fixtures.getJob("minimal"));
  });

  test("should map full config correctly", () => {
    testMapJob(Fixtures.getJob("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const job = Fixtures.getJob("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapJob(job, entityRefParser)).toThrowError();
  });

  function testMapJob(job: TsAppSpec.Job): void {
    const entityRefParser = makeRefParser("Entity", job.entities ?? []);

    const result = mapJob(job, entityRefParser);

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: {
        fn: mapExtImport(job.fn),
        executorOptions: job.performExecutorOptions,
      },
      schedule: job.schedule && mapSchedule(job.schedule),
      entities: job.entities?.map(entityRefParser),
    } satisfies AppSpec.Job);
  }
});

describe("mapCrud", () => {
  test("should map minimal config correctly", () => {
    testMapCrud(Fixtures.getCrud("minimal"));
  });

  test("should map full config correctly", () => {
    testMapCrud(Fixtures.getCrud("full"));
  });

  test("should throw if entity ref is not provided", () => {
    const crudPart = Fixtures.getCrud("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() => mapCrud(crudPart, entityRefParser)).toThrowError();
  });

  function testMapCrud(crudPart: TsAppSpec.Crud): void {
    const entityRefParser = makeRefParser("Entity", [crudPart.entity]);

    const result = mapCrud(crudPart, entityRefParser);

    expect(result).toStrictEqual({
      entity: entityRefParser(crudPart.entity),
      operations: mapCrudOperations(crudPart.operations),
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

/**
 * An `expect(value).toBeDefined()` assertion that also narrows the type of the
 * variable.
 */
function assertDefined<T>(value: T | null | undefined): asserts value is T {
  expect(value).toBeDefined();
}
