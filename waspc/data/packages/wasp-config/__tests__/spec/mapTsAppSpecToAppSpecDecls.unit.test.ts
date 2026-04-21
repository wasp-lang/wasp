import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { GET_TS_APP_SPEC } from "../../src/spec/_private.js";
import {
  makeRefParser,
  mapApp,
  mapExtImport,
  mapOperation,
  mapPage,
} from "../../src/spec/mapTsAppSpecToAppSpecDecls.js";
import { App } from "../../src/spec/publicApi/App.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
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

    const result = mapApp(tsAppSpec.app.config);

    expect(result).toStrictEqual({
      wasp: {
        version: tsAppSpec.app.config.wasp.version,
      },
      title: tsAppSpec.app.config.title,
      head: tsAppSpec.app.config.head,
      auth: undefined,
      server: undefined,
      client: undefined,
      db: undefined,
      emailSender: undefined,
      webSocket: undefined,
    } satisfies AppSpec.App);
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

  function testMapOperation(
    operation: TsAppSpec.QueryConfig,
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
