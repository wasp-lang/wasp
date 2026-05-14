







import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import {
  deriveExtImportName,
  makeRefParser,
  mapAction,
  mapApp,
  mapAuth,
  mapAuthMethods,
  mapEmailAuth,
  mapEmailFromField,
  mapEmailVerification,
  mapExternalAuth,
  mapExtImport,
  mapPage,
  mapPasswordReset,
  mapQuery,
  mapRoute,
  mapUsernameAndPassword,
} from "../../src/spec/mapApp.js";
import { app, page, route } from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("mapApp", () => {
  test("should map minimal app correctly", () => {
    const entityNames = Fixtures.getEntities("minimal");
    const app = Fixtures.getMinimalApp();

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
    const page = Fixtures.getPage("full");
    const query = Fixtures.getQuery("full");
    const emailVerifyRoute = Fixtures.getEmailVerifyRoute();
    const passwordResetRoute = Fixtures.getPasswordResetRoute();
    const authConfig = Fixtures.getAuthConfig("full");
    const entityNames = Fixtures.getEntities("full");
    const entityRefParser = makeRefParser("Entity", entityNames);

    const inputApp = app({
      name: "FullApp",
      wasp: { version: "^0.16.3" },
      title: "Mock App",
      head: ['<link rel="icon" href="/favicon.ico" />'],
      auth: authConfig,
      parts: [page, query, emailVerifyRoute, passwordResetRoute],
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
          server: undefined,
          client: undefined,
          db: undefined,
          emailSender: undefined,
          webSocket: undefined,
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
      `Conflicting configs for page "${pageName}"`,
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
      `Conflicting configs for page "${pageName}"`,
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
      `Conflicting configs for page "${pageName}"`,
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
    expect(authMethods.email.emailVerification.clientRoute).toBeDefined();
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.passwordReset.clientRoute],
      shouldError: true,
    });
  });

  test("should throw if passwordReset clientRoute ref is not provided when defined", () => {
    const authMethods = Fixtures.getAuthMethods("full");
    expect(authMethods.email.passwordReset.clientRoute).toBeDefined();
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
    const routes = overrideRoutes ?? [emailVerification.clientRoute];
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
    const routes = overrideRoutes ?? [passwordReset.clientRoute];
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

  function testMapExtImport(extImport: TsAppSpec.ExtImport): void {
    const result = mapExtImport(extImport);

    if ("import" in extImport) {
      expect(result).toStrictEqual({
        kind: "named",
        name: extImport.import,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    }
  }
});
