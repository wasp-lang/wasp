import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import * as AppSpecMapper from "../../src/spec/mapApp.js";
import { app, page, route } from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import { mapRefImportToExtImport } from "../../src/spec/refImport.js";
import * as Fixtures from "./testFixtures.js";

const mockProjectDir = "/project";

function mapRefImportForMockProjectDir(refImport: unknown) {
  return mapRefImportToExtImport(refImport, { projectRootDir: mockProjectDir });
}

const makeRefParser = AppSpecMapper.makeRefParser;

function mapApp(app: TsAppSpec.App, entityNames: string[]) {
  return AppSpecMapper.mapApp(app, {
    entityNames,
    projectRootDir: mockProjectDir,
  });
}

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
      decls: [
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

    // TODO: Reaching into `deriveRefImportName` here is not ideal — it leaks
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
          auth: AppSpecMapper.mapAuth(
            authConfig,
            entityRefParser,
            routeRefParser,
            mapRefImportForMockProjectDir,
          ),
          server: AppSpecMapper.mapServer(
            server,
            mapRefImportForMockProjectDir,
          ),
          client: AppSpecMapper.mapClient(
            client,
            mapRefImportForMockProjectDir,
          ),
          db: AppSpecMapper.mapDb(db, mapRefImportForMockProjectDir),
          emailSender: AppSpecMapper.mapEmailSender(emailSender),
          webSocket: AppSpecMapper.mapWebSocket(
            webSocket,
            mapRefImportForMockProjectDir,
          ),
        },
      },
      {
        declType: "Page",
        declName: AppSpecMapper.deriveRefImportName(page.component),
        declValue: AppSpecMapper.mapPage(page, mapRefImportForMockProjectDir),
      },
      {
        declType: "Page",
        declName: AppSpecMapper.deriveRefImportName(
          emailVerifyRoute.page.component,
        ),
        declValue: AppSpecMapper.mapPage(
          emailVerifyRoute.page,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "Page",
        declName: AppSpecMapper.deriveRefImportName(
          passwordResetRoute.page.component,
        ),
        declValue: AppSpecMapper.mapPage(
          passwordResetRoute.page,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "Route",
        declName: route.name,
        declValue: AppSpecMapper.mapRoute(route),
      },
      {
        declType: "Route",
        declName: emailVerifyRoute.name,
        declValue: AppSpecMapper.mapRoute(emailVerifyRoute),
      },
      {
        declType: "Route",
        declName: passwordResetRoute.name,
        declValue: AppSpecMapper.mapRoute(passwordResetRoute),
      },
      {
        declType: "Query",
        declName: AppSpecMapper.deriveRefImportName(query.fn),
        declValue: AppSpecMapper.mapQuery(
          query,
          entityRefParser,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "Api",
        declName: AppSpecMapper.deriveRefImportName(api.fn),
        declValue: AppSpecMapper.mapApi(
          api,
          entityRefParser,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "ApiNamespace",
        declName: AppSpecMapper.deriveRefImportName(
          apiNamespace.middlewareConfigFn,
        ),
        declValue: AppSpecMapper.mapApiNamespace(
          apiNamespace,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "Job",
        declName: AppSpecMapper.deriveRefImportName(job.fn),
        declValue: AppSpecMapper.mapJob(
          job,
          entityRefParser,
          mapRefImportForMockProjectDir,
        ),
      },
      {
        declType: "Crud",
        declName: crud.name,
        declValue: AppSpecMapper.mapCrud(
          crud,
          entityRefParser,
          mapRefImportForMockProjectDir,
        ),
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("dedups a page referenced explicitly twice", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);

    const app = Fixtures.getMinimalAppWithDecls([page1, page2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced via a route shorthand twice", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithDecls([route1, route2]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport);
    const route1 = route("Route1", "/", page2);

    const app = Fixtures.getMinimalAppWithDecls([page1, route1]);
    const decls = mapApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("throws when the same page name is produced with differing configs explicitly", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });

    const app = Fixtures.getMinimalAppWithDecls([page1, page2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs via a route shorthand twice", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithDecls([route1, route2]);

    expect(() => mapApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs explicitly and via a route shorthand", () => {
    const extImport = Fixtures.getRefImport("minimal", "default");
    const pageName = AppSpecMapper.deriveRefImportName(extImport);
    const page1 = page(extImport);
    const page2 = page(extImport, { authRequired: true });
    const route1 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithDecls([page1, route1]);

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
    const result = AppSpecMapper.mapPage(page, mapRefImportForMockProjectDir);

    expect(result).toStrictEqual({
      component: mapRefImportForMockProjectDir(page.component),
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
    const result = AppSpecMapper.mapRoute(route);

    expect(result).toStrictEqual({
      path: route.path,
      to: {
        name: AppSpecMapper.deriveRefImportName(route.page.component),
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

    expect(() =>
      AppSpecMapper.mapQuery(
        query,
        entityRefParser,
        mapRefImportForMockProjectDir,
      ),
    ).toThrowError();
  });

  function testMapQuery(query: TsAppSpec.Query): void {
    const entityRefParser = makeRefParser("Entity", query.entities ?? []);

    const result = AppSpecMapper.mapQuery(
      query,
      entityRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      fn: mapRefImportForMockProjectDir(query.fn),
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

    expect(() =>
      AppSpecMapper.mapAction(
        action,
        entityRefParser,
        mapRefImportForMockProjectDir,
      ),
    ).toThrowError();
  });

  function testMapAction(action: TsAppSpec.Action): void {
    const entityRefParser = makeRefParser("Entity", action.entities ?? []);

    const result = AppSpecMapper.mapAction(
      action,
      entityRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      fn: mapRefImportForMockProjectDir(action.fn),
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
    const entities = overrideEntities ?? [auth.userEntity];
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
        AppSpecMapper.mapAuth(
          auth,
          entityRefParser,
          routeRefParser,
          mapRefImportForMockProjectDir,
        ),
      ).toThrowError();
      return;
    }

    const result = AppSpecMapper.mapAuth(
      auth,
      entityRefParser,
      routeRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      userEntity: entityRefParser(auth.userEntity),
      methods: AppSpecMapper.mapAuthMethods(
        auth.methods,
        routeRefParser,
        mapRefImportForMockProjectDir,
      ),
      onAuthFailedRedirectTo: auth.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: auth.onAuthSucceededRedirectTo,
      onBeforeSignup:
        auth.onBeforeSignup &&
        mapRefImportForMockProjectDir(auth.onBeforeSignup),
      onAfterSignup:
        auth.onAfterSignup && mapRefImportForMockProjectDir(auth.onAfterSignup),
      onAfterEmailVerified:
        auth.onAfterEmailVerified &&
        mapRefImportForMockProjectDir(auth.onAfterEmailVerified),
      onBeforeOAuthRedirect:
        auth.onBeforeOAuthRedirect &&
        mapRefImportForMockProjectDir(auth.onBeforeOAuthRedirect),
      onBeforeLogin:
        auth.onBeforeLogin && mapRefImportForMockProjectDir(auth.onBeforeLogin),
      onAfterLogin:
        auth.onAfterLogin && mapRefImportForMockProjectDir(auth.onAfterLogin),
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
      expect(() =>
        AppSpecMapper.mapAuthMethods(
          authMethods,
          routeRefParser,
          mapRefImportForMockProjectDir,
        ),
      ).toThrowError();
      return;
    }

    const result = AppSpecMapper.mapAuthMethods(
      authMethods,
      routeRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      usernameAndPassword:
        authMethods.usernameAndPassword &&
        AppSpecMapper.mapUsernameAndPassword(
          authMethods.usernameAndPassword,
          mapRefImportForMockProjectDir,
        ),
      slack:
        authMethods.slack &&
        AppSpecMapper.mapSocialAuth(
          authMethods.slack,
          mapRefImportForMockProjectDir,
        ),
      discord:
        authMethods.discord &&
        AppSpecMapper.mapSocialAuth(
          authMethods.discord,
          mapRefImportForMockProjectDir,
        ),
      google:
        authMethods.google &&
        AppSpecMapper.mapSocialAuth(
          authMethods.google,
          mapRefImportForMockProjectDir,
        ),
      gitHub:
        authMethods.gitHub &&
        AppSpecMapper.mapSocialAuth(
          authMethods.gitHub,
          mapRefImportForMockProjectDir,
        ),
      keycloak:
        authMethods.keycloak &&
        AppSpecMapper.mapSocialAuth(
          authMethods.keycloak,
          mapRefImportForMockProjectDir,
        ),
      microsoft:
        authMethods.microsoft &&
        AppSpecMapper.mapSocialAuth(
          authMethods.microsoft,
          mapRefImportForMockProjectDir,
        ),
      email:
        authMethods.email &&
        AppSpecMapper.mapEmailAuth(
          authMethods.email,
          routeRefParser,
          mapRefImportForMockProjectDir,
        ),
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
      expect(() =>
        AppSpecMapper.mapEmailAuth(
          emailAuth,
          routeRefParser,
          mapRefImportForMockProjectDir,
        ),
      ).toThrowError();
      return;
    }

    const result = AppSpecMapper.mapEmailAuth(
      emailAuth,
      routeRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      userSignupFields:
        emailAuth.userSignupFields &&
        mapRefImportForMockProjectDir(emailAuth.userSignupFields),
      fromField: AppSpecMapper.mapEmailFromField(emailAuth.fromField),
      emailVerification: AppSpecMapper.mapEmailFlow(
        emailAuth.emailVerification,
        routeRefParser,
        mapRefImportForMockProjectDir,
      ),
      passwordReset: AppSpecMapper.mapEmailFlow(
        emailAuth.passwordReset,
        routeRefParser,
        mapRefImportForMockProjectDir,
      ),
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
      expect(() =>
        AppSpecMapper.mapEmailFlow(
          emailFlow,
          routeRefParser,
          mapRefImportForMockProjectDir,
        ),
      ).toThrowError();
      return;
    }

    const result = AppSpecMapper.mapEmailFlow(
      emailFlow,
      routeRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(emailFlow.clientRoute),
      getEmailContentFn:
        emailFlow.getEmailContentFn &&
        mapRefImportForMockProjectDir(emailFlow.getEmailContentFn),
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
    const result = AppSpecMapper.mapUsernameAndPassword(
      usernameAndPassword,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      userSignupFields:
        usernameAndPassword.userSignupFields &&
        mapRefImportForMockProjectDir(usernameAndPassword.userSignupFields),
    } satisfies AppSpec.UsernameAndPasswordConfig);
  }
});

describe("mapSocialAuth", () => {
  test("should map minimal config correctly", () => {
    testMapSocialAuth(Fixtures.getSocialAuthConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapSocialAuth(Fixtures.getSocialAuthConfig("full"));
  });

  function testMapSocialAuth(socialAuth: TsAppSpec.SocialAuthConfig): void {
    const result = AppSpecMapper.mapSocialAuth(
      socialAuth,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      configFn:
        socialAuth.configFn &&
        mapRefImportForMockProjectDir(socialAuth.configFn),
      userSignupFields:
        socialAuth.userSignupFields &&
        mapRefImportForMockProjectDir(socialAuth.userSignupFields),
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

    expect(() =>
      AppSpecMapper.mapApi(api, entityRefParser, mapRefImportForMockProjectDir),
    ).toThrowError();
  });

  function testMapApi(api: TsAppSpec.Api): void {
    const entityRefParser = makeRefParser("Entity", api.entities ?? []);

    const result = AppSpecMapper.mapApi(
      api,
      entityRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      fn: mapRefImportForMockProjectDir(api.fn),
      middlewareConfigFn:
        api.middlewareConfigFn &&
        mapRefImportForMockProjectDir(api.middlewareConfigFn),
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
    const result = AppSpecMapper.mapApiNamespace(
      apiNamespace,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      middlewareConfigFn: mapRefImportForMockProjectDir(
        apiNamespace.middlewareConfigFn,
      ),
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
    const result = AppSpecMapper.mapServer(
      server,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      setupFn: server.setupFn && mapRefImportForMockProjectDir(server.setupFn),
      middlewareConfigFn:
        server.middlewareConfigFn &&
        mapRefImportForMockProjectDir(server.middlewareConfigFn),
      envValidationSchema:
        server.envValidationSchema &&
        mapRefImportForMockProjectDir(server.envValidationSchema),
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
    const result = AppSpecMapper.mapClient(
      client,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      rootComponent:
        client.rootComponent &&
        mapRefImportForMockProjectDir(client.rootComponent),
      setupFn: client.setupFn && mapRefImportForMockProjectDir(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema:
        client.envValidationSchema &&
        mapRefImportForMockProjectDir(client.envValidationSchema),
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
    const result = AppSpecMapper.mapDb(db, mapRefImportForMockProjectDir);

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapRefImportForMockProjectDir(seed)),
      prismaSetupFn:
        db.prismaSetupFn && mapRefImportForMockProjectDir(db.prismaSetupFn),
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
    const result = AppSpecMapper.mapEmailSender(emailSender);

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom:
        emailSender.defaultFrom &&
        AppSpecMapper.mapEmailFromField(emailSender.defaultFrom),
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
    const result = AppSpecMapper.mapEmailFromField(emailFromField);

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
    const result = AppSpecMapper.mapWebSocket(
      webSocket,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      fn: mapRefImportForMockProjectDir(webSocket.fn),
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

    expect(() =>
      AppSpecMapper.mapJob(job, entityRefParser, mapRefImportForMockProjectDir),
    ).toThrowError();
  });

  function testMapJob(job: TsAppSpec.Job): void {
    const entityRefParser = makeRefParser("Entity", job.entities ?? []);

    const result = AppSpecMapper.mapJob(
      job,
      entityRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: {
        fn: mapRefImportForMockProjectDir(job.fn),
        executorOptions: job.performExecutorOptions,
      },
      schedule: job.schedule && AppSpecMapper.mapSchedule(job.schedule),
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
    const crudDecl = Fixtures.getCrud("full");
    const entityRefParser = makeRefParser("Entity", []);

    expect(() =>
      AppSpecMapper.mapCrud(
        crudDecl,
        entityRefParser,
        mapRefImportForMockProjectDir,
      ),
    ).toThrowError();
  });

  function testMapCrud(crudDecl: TsAppSpec.Crud): void {
    const entityRefParser = makeRefParser("Entity", [crudDecl.entity]);

    const result = AppSpecMapper.mapCrud(
      crudDecl,
      entityRefParser,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      entity: entityRefParser(crudDecl.entity),
      operations: AppSpecMapper.mapCrudOperations(
        crudDecl.operations,
        mapRefImportForMockProjectDir,
      ),
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
    const result = AppSpecMapper.mapCrudOperations(
      crudOperations,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      get:
        crudOperations.get &&
        AppSpecMapper.mapCrudOperationOptions(
          crudOperations.get,
          mapRefImportForMockProjectDir,
        ),
      getAll:
        crudOperations.getAll &&
        AppSpecMapper.mapCrudOperationOptions(
          crudOperations.getAll,
          mapRefImportForMockProjectDir,
        ),
      create:
        crudOperations.create &&
        AppSpecMapper.mapCrudOperationOptions(
          crudOperations.create,
          mapRefImportForMockProjectDir,
        ),
      update:
        crudOperations.update &&
        AppSpecMapper.mapCrudOperationOptions(
          crudOperations.update,
          mapRefImportForMockProjectDir,
        ),
      delete:
        crudOperations.delete &&
        AppSpecMapper.mapCrudOperationOptions(
          crudOperations.delete,
          mapRefImportForMockProjectDir,
        ),
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
    const result = AppSpecMapper.mapCrudOperationOptions(
      crudOperationOptions,
      mapRefImportForMockProjectDir,
    );

    expect(result).toStrictEqual({
      isPublic: crudOperationOptions.isPublic,
      overrideFn:
        crudOperationOptions.overrideFn &&
        mapRefImportForMockProjectDir(crudOperationOptions.overrideFn),
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
    const result = AppSpecMapper.mapSchedule(schedule);

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
