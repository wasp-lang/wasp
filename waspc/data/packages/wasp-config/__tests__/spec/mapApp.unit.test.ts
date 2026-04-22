import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import {
  deriveExtImportName,
  makeRefParser,
  mapApp,
  mapExtImport,
  mapPage,
  mapQuery,
} from "../../src/spec/mapApp.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("mapApp", () => {
  test("should map minimal app correctly", () => {
    const spec: TsAppSpec.App = Fixtures.getApp("minimal");
    const entityNames = Fixtures.getEntities("minimal");

    const result = mapApp(spec, entityNames);

    expect(result).toStrictEqual([
      {
        declType: "App",
        declName: spec.name,
        declValue: {
          wasp: spec.wasp,
          title: spec.title,
          head: spec.head,
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
    const spec: TsAppSpec.App = Fixtures.getApp("full");
    const entityNames = Fixtures.getEntities("full");
    const page: TsAppSpec.Page = Fixtures.getPage("full");
    const query: TsAppSpec.Query = Fixtures.getQuery("full");
    const entityRefParser = makeRefParser("Entity", entityNames);

    const result = mapApp(spec, entityNames);

    // TODO: Reaching into `deriveExtImportName` here is not ideal — it leaks
    // an orchestrator-internal helper into the test. Revisit once we have a
    // higher-level name-derivation system: either a part-agnostic
    // `deriveDeclName(part)`, or a part-specific dispatch (mirroring how we
    // have one mapper per kind). The test should delegate to whatever
    // surfaces.
    expect(result).toStrictEqual([
      {
        declType: "App",
        declName: spec.name,
        declValue: {
          wasp: spec.wasp,
          title: spec.title,
          head: spec.head,
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

  // TODO: dedup pages referenced multiple times in parts → one Page decl.
  // TODO: two pages deriving the same name with different configs → throw.
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
