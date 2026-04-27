import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import {
  dedupePageDecls,
  deriveExtImportName,
  deriveRouteName,
  makeRefParser,
  mapAction,
  mapApp,
  mapExtImport,
  mapPage,
  mapQuery,
  mapRoute,
  normalizeRoutePage,
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
    const entityNames = Fixtures.getEntities("full");
    const entityRefParser = makeRefParser("Entity", entityNames);

    const inputApp = app({
      name: "FullApp",
      wasp: { version: "^0.16.3" },
      title: "Mock App",
      head: ['<link rel="icon" href="/favicon.ico" />'],
      parts: [page, query],
    });

    const result = mapApp(inputApp, entityNames);

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
          auth: undefined,
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
        declType: "Query",
        declName: deriveExtImportName(query.fn),
        declValue: mapQuery(query, entityRefParser),
      },
    ] satisfies AppSpec.Decl[]);
  });

  test("dedups a page referenced both explicitly and via a route shorthand", () => {
    const aboutPage = page({ import: "AboutPage", from: "@src/About" });
    const spec = app({
      name: "TodoApp",
      wasp: { version: "^0.16.3" },
      title: "Todo",
      parts: [aboutPage, route("/about", aboutPage)],
    });

    const decls = mapApp(spec, []);

    const pageNames = decls
      .filter((d) => d.declType === "Page")
      .map((d) => d.declName);
    expect(pageNames).toEqual(["AboutPage"]);
  });

  test("throws when the same page name is produced with differing configs", () => {
    const spec = app({
      name: "TodoApp",
      wasp: { version: "^0.16.3" },
      title: "Todo",
      parts: [
        page(
          { import: "AboutPage", from: "@src/About" },
          { authRequired: true },
        ),
        route("/about", { import: "AboutPage", from: "@src/About" }),
      ],
    });

    expect(() => mapApp(spec, [])).toThrow(
      /Conflicting configs for page "AboutPage"/,
    );
  });

  // TODO: duplicate query name → throw.
  // TODO: duplicate route name → throw (distinct paths can still derive the same name).
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

    const expectedPage = normalizeRoutePage(route.page);
    const expectedPageName = deriveExtImportName(expectedPage.component);

    expect(result.route).toStrictEqual({
      path: route.path,
      to: { name: expectedPageName, declType: "Page" },
      prerender: undefined,
      lazy: undefined,
    } satisfies AppSpec.Route);

    expect(result.page).toStrictEqual(mapPage(expectedPage));
  }
});

describe("deriveRouteName", () => {
  const cases: ReadonlyArray<readonly [string, string]> = [
    ["/", "RootRoute"],
    ["", "RootRoute"],
    ["/about", "AboutRoute"],
    ["/foo/bar", "FooBarRoute"],
    ["/users/:id", "UsersIdRoute"],
    ["/users/:userId/posts/:postId", "UsersUserIdPostsPostIdRoute"],
    ["/sign-up", "SignUpRoute"],
    ["/password-reset/:token", "PasswordResetTokenRoute"],
  ];

  test.each(cases)("derives %s → %s", (path, expected) => {
    expect(deriveRouteName(path)).toBe(expected);
  });
});

describe("dedupePageDecls", () => {
  const homePage: AppSpec.GetDeclForType<"Page"> = {
    declType: "Page",
    declName: "HomePage",
    declValue: {
      component: { kind: "default", name: "HomePage", path: "@src/Home" },
      authRequired: undefined,
    },
  };
  const homePageAuth: AppSpec.GetDeclForType<"Page"> = {
    ...homePage,
    declValue: { ...homePage.declValue, authRequired: true },
  };
  const aboutPage: AppSpec.GetDeclForType<"Page"> = {
    declType: "Page",
    declName: "AboutPage",
    declValue: {
      component: { kind: "named", name: "AboutPage", path: "@src/About" },
      authRequired: undefined,
    },
  };

  test("returns distinct decls unchanged", () => {
    expect(dedupePageDecls([homePage, aboutPage])).toStrictEqual([
      homePage,
      aboutPage,
    ]);
  });

  test("collapses duplicate decls with matching configs into one", () => {
    expect(dedupePageDecls([homePage, aboutPage, homePage])).toStrictEqual([
      homePage,
      aboutPage,
    ]);
  });

  test("throws when decls share a name but differ in config", () => {
    expect(() => dedupePageDecls([homePage, homePageAuth])).toThrow(
      /Conflicting configs for page "HomePage"/,
    );
  });
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
