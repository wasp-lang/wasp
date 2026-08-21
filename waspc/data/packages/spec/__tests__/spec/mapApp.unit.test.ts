import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { normalizePrerender } from "../../src/normalizePrerender.js";
import * as AppSpecMapper from "../../src/spec/mapper/app.js";
import {
  type AppMapperContext,
  makeRefParser,
} from "../../src/spec/mapper/context.js";
import { convertWaspSpecToAppSpec } from "../../src/spec/mapper/index.js";
import * as SpecElementMapper from "../../src/spec/mapper/specElements.js";
import {
  api,
  app,
  page,
  query,
  route,
} from "../../src/spec/publicApi/index.js";
import * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";
import {
  getRefObjectDeclarationName,
  mapRefObject,
} from "../../src/spec/refObject.js";
import * as Fixtures from "./testFixtures.js";

function mapRefObjectForMockProjectDir(refObject: unknown) {
  return mapRefObject(refObject);
}

function makeMapperContext({
  entityNames = [],
  routeNames = [],
}: {
  entityNames?: string[];
  routeNames?: string[];
} = {}): AppMapperContext {
  const ctx: AppMapperContext = {
    resolveEntityRef: makeRefParser("Entity", entityNames),
    resolveRouteRef: makeRefParser("Route", routeNames),
    parseRefObject: mapRefObjectForMockProjectDir,
    // For testing, the individual mapper functions only need the `AppSpec.Ref`.
    collectSpecElement<SpecElement extends WaspSpec.SpecElement>(
      specElement: SpecElement,
    ) {
      const decl = SpecElementMapper.mapSpecElement(specElement, ctx);
      return SpecElementMapper.declToRef<SpecElement>(decl);
    },
  };
  return ctx;
}

function getSpecElementDeclName(specElement: WaspSpec.SpecElement): string {
  return SpecElementMapper.mapSpecElement(specElement, makeMapperContext())
    .declName;
}

function mapMockApp(app: WaspSpec.App, entityNames: string[]) {
  return convertWaspSpecToAppSpec(app, {
    entityNames,
  });
}

describe("convertWaspSpecToAppSpec", () => {
  test("should map minimal app correctly", () => {
    const entityNames = Fixtures.getEntities("minimal");
    const app = Fixtures.getApp("minimal");

    const decls = mapMockApp(app, entityNames);

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
      spec: [
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

    const result = mapMockApp(inputApp, entityNames);

    const ctx = makeMapperContext({
      entityNames,
      routeNames: [emailVerifyRoute.name, passwordResetRoute.name],
    });

    // Decls come out in registration order: each `spec` element in listing
    // order, with a route's page registered right before the route itself.
    expect(result).toStrictEqual([
      {
        declType: "App",
        declName: inputApp.name,
        declValue: {
          wasp: inputApp.wasp,
          title: inputApp.title,
          head: inputApp.head,
          auth: AppSpecMapper.mapAuth(authConfig, ctx),
          server: AppSpecMapper.mapServer(server, ctx),
          client: AppSpecMapper.mapClient(client, ctx),
          db: AppSpecMapper.mapDb(db, ctx),
          emailSender: AppSpecMapper.mapEmailSender(emailSender),
          webSocket: AppSpecMapper.mapWebSocket(webSocket, ctx),
        },
      },
      SpecElementMapper.mapPageSpec(page, ctx),
      SpecElementMapper.mapRouteSpec(route, ctx),
      SpecElementMapper.mapQuerySpec(query, ctx),
      SpecElementMapper.mapApiSpec(api, ctx),
      SpecElementMapper.mapApiNamespaceSpec(apiNamespace, ctx),
      SpecElementMapper.mapJobSpec(job, ctx),
      SpecElementMapper.mapCrudSpec(crud, ctx),
      SpecElementMapper.mapPageSpec(emailVerifyRoute.page, ctx),
      SpecElementMapper.mapRouteSpec(emailVerifyRoute, ctx),
      SpecElementMapper.mapPageSpec(passwordResetRoute.page, ctx),
      SpecElementMapper.mapRouteSpec(passwordResetRoute, ctx),
    ] satisfies AppSpec.Decl[]);
  });

  test("dedups a page referenced explicitly twice", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject);

    const app = Fixtures.getMinimalAppWithSpec([page1, page2]);
    const decls = mapMockApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced via a route shorthand twice", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject);
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithSpec([route1, route2]);
    const decls = mapMockApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("dedups a page referenced explicitly and via a route shorthand", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject);
    const route1 = route("Route1", "/", page2);

    const app = Fixtures.getMinimalAppWithSpec([page1, route1]);
    const decls = mapMockApp(app, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual([pageName]);
  });

  test("throws when the same page name is produced with differing configs explicitly", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject, { authRequired: true });

    const app = Fixtures.getMinimalAppWithSpec([page1, page2]);

    expect(() => mapMockApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs via a route shorthand twice", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject, { authRequired: true });
    const route1 = route("Route1", "/", page1);
    const route2 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithSpec([route1, route2]);

    expect(() => mapMockApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("throws when the same page name is produced with differing configs explicitly and via a route shorthand", () => {
    const refObject = Fixtures.getRefObject("minimal", "default");
    const page1 = page(refObject);
    const pageName = getSpecElementDeclName(page1);
    const page2 = page(refObject, { authRequired: true });
    const route1 = route("Route2", "/", page2);

    const app = Fixtures.getMinimalAppWithSpec([page1, route1]);

    expect(() => mapMockApp(app, [])).toThrow(
      `Conflicting configurations for the page \`${pageName}\``,
    );
  });

  test("allows decls of different types to share a name", () => {
    const refObject = Fixtures.getRefObject("minimal", "named");
    const query1 = query(refObject);
    const api1 = api("GET", "/foo/bar", refObject);

    const app = Fixtures.getMinimalAppWithSpec([query1, api1]);
    const decls = mapMockApp(app, []);

    const declTypes = decls.map((d) => d.declType);
    expect(declTypes).toEqual(["App", "Query", "Api"]);
    expect(getSpecElementDeclName(query1)).toBe(getSpecElementDeclName(api1));
  });

  test("dedups a query listed twice with identical configs", () => {
    const refObject = Fixtures.getRefObject("minimal", "named");
    const query1 = query(refObject);
    const query2 = query(refObject);

    const app = Fixtures.getMinimalAppWithSpec([query1, query2]);
    const decls = mapMockApp(app, []);

    const queryNames = decls
      .filter((d) => d.declType === "Query")
      .map((d) => d.declName);
    expect(queryNames).toEqual([getSpecElementDeclName(query1)]);
  });

  test("throws when two queries share a name with differing configs", () => {
    const refObject = Fixtures.getRefObject("minimal", "named");
    const query1 = query(refObject);
    const query2 = query(refObject, { auth: true });

    const app = Fixtures.getMinimalAppWithSpec([query1, query2]);

    expect(() => mapMockApp(app, [])).toThrow(
      `Conflicting configurations for the query \`${getSpecElementDeclName(query1)}\``,
    );
  });
});

describe("mapPage", () => {
  test("should map minimal config correctly", () => {
    testMapPage(Fixtures.getPage("minimal"));
  });

  test("should map full config correctly", () => {
    testMapPage(Fixtures.getPage("full"));
  });

  function testMapPage(page: WaspSpec.Page): void {
    const ctx = makeMapperContext();
    const result = SpecElementMapper.mapPageSpec(page, ctx);

    expect(result).toStrictEqual({
      declType: "Page",
      declName: getRefObjectDeclarationName(page.component),
      declValue: {
        component: mapRefObjectForMockProjectDir(page.component),
        authRequired: page.authRequired,
      },
    } satisfies AppSpec.GetDeclForType<"Page">);
  }
});

describe("mapRoute", () => {
  test("should map minimal config correctly", () => {
    testMapRoute(Fixtures.getRoute("minimal"));
  });

  test("should map full config correctly", () => {
    testMapRoute(Fixtures.getRoute("full"));
  });

  test("should expand prerender: true to the route's own path", () => {
    const result = SpecElementMapper.mapRouteSpec(
      route("LandingRoute", "/landing", Fixtures.getPage("minimal"), {
        prerender: true,
      }),
      makeMapperContext(),
    );
    expect(result.declValue.prerender).toEqual(["/landing"]);
  });

  function testMapRoute(route: WaspSpec.Route): void {
    const ctx = makeMapperContext();
    const result = SpecElementMapper.mapRouteSpec(route, ctx);

    expect(result).toStrictEqual({
      declType: "Route",
      declName: route.name,
      declValue: {
        path: route.path,
        to: {
          name: getRefObjectDeclarationName(route.page.component),
          declType: "Page",
        },
        prerender: normalizePrerender(route.prerender, route.path),
        lazy: route.lazy,
      },
    } satisfies AppSpec.GetDeclForType<"Route">);
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
    const ctx = makeMapperContext({ entityNames: [] });

    expect(() => SpecElementMapper.mapQuerySpec(query, ctx)).toThrow();
  });

  function testMapQuery(query: WaspSpec.Query): void {
    const ctx = makeMapperContext({ entityNames: query.entities ?? [] });

    const result = SpecElementMapper.mapQuerySpec(query, ctx);

    expect(result).toStrictEqual({
      declType: "Query",
      declName: getRefObjectDeclarationName(query.fn),
      declValue: {
        fn: mapRefObjectForMockProjectDir(query.fn),
        entities: query.entities?.map(ctx.resolveEntityRef),
        auth: query.auth,
      },
    } satisfies AppSpec.GetDeclForType<"Query">);
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
    const ctx = makeMapperContext({ entityNames: [] });

    expect(() => SpecElementMapper.mapActionSpec(action, ctx)).toThrow();
  });

  function testMapAction(action: WaspSpec.Action): void {
    const ctx = makeMapperContext({ entityNames: action.entities ?? [] });

    const result = SpecElementMapper.mapActionSpec(action, ctx);

    expect(result).toStrictEqual({
      declType: "Action",
      declName: getRefObjectDeclarationName(action.fn),
      declValue: {
        fn: mapRefObjectForMockProjectDir(action.fn),
        entities: action.entities?.map(ctx.resolveEntityRef),
        auth: action.auth,
      },
    } satisfies AppSpec.GetDeclForType<"Action">);
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
    auth: WaspSpec.Auth,
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
    const ctx = makeMapperContext({
      entityNames: entities,
      routeNames: routes,
    });

    if (shouldError) {
      expect(() => AppSpecMapper.mapAuth(auth, ctx)).toThrow();
      return;
    }

    const result = AppSpecMapper.mapAuth(auth, ctx);

    expect(result).toStrictEqual({
      userEntity: ctx.resolveEntityRef(auth.userEntity),
      methods: AppSpecMapper.mapAuthMethods(auth.methods, ctx),
      onAuthFailedRedirectTo: auth.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: auth.onAuthSucceededRedirectTo,
      onBeforeSignup:
        auth.onBeforeSignup &&
        mapRefObjectForMockProjectDir(auth.onBeforeSignup),
      onAfterSignup:
        auth.onAfterSignup && mapRefObjectForMockProjectDir(auth.onAfterSignup),
      onAfterEmailVerified:
        auth.onAfterEmailVerified &&
        mapRefObjectForMockProjectDir(auth.onAfterEmailVerified),
      onBeforeOAuthRedirect:
        auth.onBeforeOAuthRedirect &&
        mapRefObjectForMockProjectDir(auth.onBeforeOAuthRedirect),
      onBeforeLogin:
        auth.onBeforeLogin && mapRefObjectForMockProjectDir(auth.onBeforeLogin),
      onAfterLogin:
        auth.onAfterLogin && mapRefObjectForMockProjectDir(auth.onAfterLogin),
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
    authMethods: WaspSpec.AuthMethods,
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
    const ctx = makeMapperContext({ routeNames: routes });

    if (shouldError) {
      expect(() => AppSpecMapper.mapAuthMethods(authMethods, ctx)).toThrow();
      return;
    }

    const result = AppSpecMapper.mapAuthMethods(authMethods, ctx);

    expect(result).toStrictEqual({
      usernameAndPassword:
        authMethods.usernameAndPassword &&
        AppSpecMapper.mapUsernameAndPassword(
          authMethods.usernameAndPassword,
          ctx,
        ),
      slack:
        authMethods.slack &&
        AppSpecMapper.mapSocialAuth(authMethods.slack, ctx),
      discord:
        authMethods.discord &&
        AppSpecMapper.mapSocialAuth(authMethods.discord, ctx),
      google:
        authMethods.google &&
        AppSpecMapper.mapSocialAuth(authMethods.google, ctx),
      gitHub:
        authMethods.gitHub &&
        AppSpecMapper.mapSocialAuth(authMethods.gitHub, ctx),
      keycloak:
        authMethods.keycloak &&
        AppSpecMapper.mapSocialAuth(authMethods.keycloak, ctx),
      microsoft:
        authMethods.microsoft &&
        AppSpecMapper.mapSocialAuth(authMethods.microsoft, ctx),
      email:
        authMethods.email && AppSpecMapper.mapEmailAuth(authMethods.email, ctx),
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
    emailAuth: WaspSpec.EmailAuthConfig,
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
    const ctx = makeMapperContext({ routeNames: routes });

    if (shouldError) {
      expect(() => AppSpecMapper.mapEmailAuth(emailAuth, ctx)).toThrow();
      return;
    }

    const result = AppSpecMapper.mapEmailAuth(emailAuth, ctx);

    expect(result).toStrictEqual({
      userSignupFields:
        emailAuth.userSignupFields &&
        mapRefObjectForMockProjectDir(emailAuth.userSignupFields),
      fromField: AppSpecMapper.mapEmailFromField(emailAuth.fromField),
      emailVerification: AppSpecMapper.mapEmailFlow(
        emailAuth.emailVerification,
        ctx,
      ),
      passwordReset: AppSpecMapper.mapEmailFlow(emailAuth.passwordReset, ctx),
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
    emailFlow: WaspSpec.EmailFlowConfig,
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
    const ctx = makeMapperContext({ routeNames: routes });

    if (shouldError) {
      expect(() => AppSpecMapper.mapEmailFlow(emailFlow, ctx)).toThrow();
      return;
    }

    const result = AppSpecMapper.mapEmailFlow(emailFlow, ctx);

    expect(result).toStrictEqual({
      clientRoute: ctx.resolveRouteRef(emailFlow.clientRoute),
      getEmailContentFn:
        emailFlow.getEmailContentFn &&
        mapRefObjectForMockProjectDir(emailFlow.getEmailContentFn),
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
    usernameAndPassword: WaspSpec.UsernameAndPasswordConfig,
  ): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapUsernameAndPassword(
      usernameAndPassword,
      ctx,
    );

    expect(result).toStrictEqual({
      userSignupFields:
        usernameAndPassword.userSignupFields &&
        mapRefObjectForMockProjectDir(usernameAndPassword.userSignupFields),
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

  function testMapSocialAuth(socialAuth: WaspSpec.SocialAuthConfig): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapSocialAuth(socialAuth, ctx);

    expect(result).toStrictEqual({
      configFn:
        socialAuth.configFn &&
        mapRefObjectForMockProjectDir(socialAuth.configFn),
      userSignupFields:
        socialAuth.userSignupFields &&
        mapRefObjectForMockProjectDir(socialAuth.userSignupFields),
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
    const ctx = makeMapperContext({ entityNames: [] });

    expect(() => SpecElementMapper.mapApiSpec(api, ctx)).toThrow();
  });

  function testMapApi(api: WaspSpec.Api): void {
    const ctx = makeMapperContext({ entityNames: api.entities ?? [] });

    const result = SpecElementMapper.mapApiSpec(api, ctx);

    expect(result).toStrictEqual({
      declType: "Api",
      declName: getRefObjectDeclarationName(api.fn),
      declValue: {
        fn: mapRefObjectForMockProjectDir(api.fn),
        middlewareConfigFn:
          api.middlewareConfigFn &&
          mapRefObjectForMockProjectDir(api.middlewareConfigFn),
        entities: api.entities?.map(ctx.resolveEntityRef),
        httpRoute: [api.method, api.path],
        auth: api.auth,
      },
    } satisfies AppSpec.GetDeclForType<"Api">);
  }
});

describe("mapApiNamespace", () => {
  test("should map minimal config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespace("minimal"));
  });

  test("should map full config correctly", () => {
    testMapApiNamespace(Fixtures.getApiNamespace("full"));
  });

  function testMapApiNamespace(apiNamespace: WaspSpec.ApiNamespace): void {
    const ctx = makeMapperContext();
    const result = SpecElementMapper.mapApiNamespaceSpec(apiNamespace, ctx);

    expect(result).toStrictEqual({
      declType: "ApiNamespace",
      declName: getRefObjectDeclarationName(apiNamespace.middlewareConfigFn),
      declValue: {
        middlewareConfigFn: mapRefObjectForMockProjectDir(
          apiNamespace.middlewareConfigFn,
        ),
        path: apiNamespace.path,
      },
    } satisfies AppSpec.GetDeclForType<"ApiNamespace">);
  }
});

describe("mapServer", () => {
  test("should map minimal config correctly", () => {
    testMapServer(Fixtures.getServerConfig("minimal"));
  });

  test("should map full config correctly", () => {
    testMapServer(Fixtures.getServerConfig("full"));
  });

  function testMapServer(server: WaspSpec.Server): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapServer(server, ctx);

    expect(result).toStrictEqual({
      setupFn: server.setupFn && mapRefObjectForMockProjectDir(server.setupFn),
      middlewareConfigFn:
        server.middlewareConfigFn &&
        mapRefObjectForMockProjectDir(server.middlewareConfigFn),
      envValidationSchema:
        server.envValidationSchema &&
        mapRefObjectForMockProjectDir(server.envValidationSchema),
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

  function testMapClient(client: WaspSpec.Client): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapClient(client, ctx);

    expect(result).toStrictEqual({
      rootComponent:
        client.rootComponent &&
        mapRefObjectForMockProjectDir(client.rootComponent),
      setupFn: client.setupFn && mapRefObjectForMockProjectDir(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema:
        client.envValidationSchema &&
        mapRefObjectForMockProjectDir(client.envValidationSchema),
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

  function testMapDb(db: WaspSpec.Db): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapDb(db, ctx);

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapRefObjectForMockProjectDir(seed)),
      prismaSetupFn:
        db.prismaSetupFn && mapRefObjectForMockProjectDir(db.prismaSetupFn),
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

  function testMapEmailSender(emailSender: WaspSpec.EmailSender): void {
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
    emailFromField: WaspSpec.EmailFromField,
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

  function testMapWebSocket(webSocket: WaspSpec.WebSocket): void {
    const ctx = makeMapperContext();
    const result = AppSpecMapper.mapWebSocket(webSocket, ctx);

    expect(result).toStrictEqual({
      fn: mapRefObjectForMockProjectDir(webSocket.fn),
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
    const ctx = makeMapperContext({ entityNames: [] });

    expect(() => SpecElementMapper.mapJobSpec(job, ctx)).toThrow();
  });

  function testMapJob(job: WaspSpec.Job): void {
    const ctx = makeMapperContext({ entityNames: job.entities ?? [] });

    const result = SpecElementMapper.mapJobSpec(job, ctx);

    expect(result).toStrictEqual({
      declType: "Job",
      declName: getRefObjectDeclarationName(job.fn),
      declValue: {
        executor: job.executor,
        perform: {
          fn: mapRefObjectForMockProjectDir(job.fn),
          executorOptions: job.performExecutorOptions,
        },
        schedule: job.schedule && SpecElementMapper.mapSchedule(job.schedule),
        entities: job.entities?.map(ctx.resolveEntityRef),
      },
    } satisfies AppSpec.GetDeclForType<"Job">);
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
    const ctx = makeMapperContext({ entityNames: [] });

    expect(() => SpecElementMapper.mapCrudSpec(crudDecl, ctx)).toThrow();
  });

  function testMapCrud(crudDecl: WaspSpec.Crud): void {
    const ctx = makeMapperContext({ entityNames: [crudDecl.entity] });

    const result = SpecElementMapper.mapCrudSpec(crudDecl, ctx);

    expect(result).toStrictEqual({
      declType: "Crud",
      declName: crudDecl.name,
      declValue: {
        entity: ctx.resolveEntityRef(crudDecl.entity),
        operations: SpecElementMapper.mapCrudOperations(
          crudDecl.operations,
          ctx,
        ),
      },
    } satisfies AppSpec.GetDeclForType<"Crud">);
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
    crudOperations: WaspSpec.CrudOperations,
  ): void {
    const ctx = makeMapperContext();
    const result = SpecElementMapper.mapCrudOperations(crudOperations, ctx);

    expect(result).toStrictEqual({
      get:
        crudOperations.get &&
        SpecElementMapper.mapCrudOperationOptions(crudOperations.get, ctx),
      getAll:
        crudOperations.getAll &&
        SpecElementMapper.mapCrudOperationOptions(crudOperations.getAll, ctx),
      create:
        crudOperations.create &&
        SpecElementMapper.mapCrudOperationOptions(crudOperations.create, ctx),
      update:
        crudOperations.update &&
        SpecElementMapper.mapCrudOperationOptions(crudOperations.update, ctx),
      delete:
        crudOperations.delete &&
        SpecElementMapper.mapCrudOperationOptions(crudOperations.delete, ctx),
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
    crudOperationOptions: WaspSpec.CrudOperationOptions,
  ): void {
    const ctx = makeMapperContext();
    const result = SpecElementMapper.mapCrudOperationOptions(
      crudOperationOptions,
      ctx,
    );

    expect(result).toStrictEqual({
      isPublic: crudOperationOptions.isPublic,
      overrideFn:
        crudOperationOptions.overrideFn &&
        mapRefObjectForMockProjectDir(crudOperationOptions.overrideFn),
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

  function testMapSchedule(schedule: WaspSpec.Schedule): void {
    const result = SpecElementMapper.mapSchedule(schedule);

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
