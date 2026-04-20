import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import {
  deriveExtImportName,
  makeRefParser,
  mapExtImport,
  mapPage,
  mapQuery,
  mapRoute,
  normalizeRoutePage,
} from "../../src/spec/mapTsAppSpecToAppSpecDecls.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

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
