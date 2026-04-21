/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, expect, test } from "vitest";
import * as AppSpec from "../../src/appSpec.js";
import { GET_TS_APP_SPEC } from "../../src/spec/_private.js";
import {
  makeRefParser,
  mapApp,
  mapOperation,
  mapPage,
  mapTsAppSpecToAppSpecDecls,
} from "../../src/spec/mapTsAppSpecToAppSpecDecls.js";
import * as Fixtures from "./testFixtures.js";

describe("mapTsAppSpecToAppSpecDecls", () => {
  test("should map full app using mapping functions correctly", () => {
    const { appConfigName, app } = Fixtures.createApp("full");
    const tsAppSpec = app[GET_TS_APP_SPEC]();
    const entities = Fixtures.getEntities("full");
    const entityRefParser = makeRefParser("Entity", entities);
    const appDeclType = "App";

    const result = mapTsAppSpecToAppSpecDecls(tsAppSpec, entities);

    const appDecl = getDecl(result, appDeclType, appConfigName);
    expect(appDecl).toStrictEqual({
      declType: appDeclType,
      declName: appConfigName,
      declValue: mapApp(tsAppSpec.app.config),
    });
    expectCorrectDeclMapping({
      declType: "Page",
      decls: tsAppSpec.pages,
      expectedMapping: {
        function: mapPage,
      },
      actualMapping: result,
    });
    expectCorrectDeclMapping({
      declType: "Query",
      decls: tsAppSpec.queries,
      expectedMapping: {
        function: mapOperation,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    });
  });

  test("should map minimal app using mapping functions correctly", () => {
    const { appConfigName, app } = Fixtures.createApp("minimal");
    const tsAppSpec = app[GET_TS_APP_SPEC]();
    const entities = Fixtures.getEntities("minimal");
    const appDeclType = "App";

    const result = mapTsAppSpecToAppSpecDecls(tsAppSpec, entities);

    const appDecl = getDecl(result, appDeclType, appConfigName);
    expect(appDecl).toStrictEqual({
      declType: appDeclType,
      declName: appConfigName,
      declValue: mapApp(tsAppSpec.app.config),
    });
  });

  function expectCorrectDeclMapping({
    declType,
    decls,
    expectedMapping,
    actualMapping,
  }: {
    declType: keyof AppSpec.DeclTypeToValue;
    decls: Map<string, any>;
    expectedMapping: {
      function: (item: any, ...args: any[]) => any;
      extraArgs?: any[];
    };
    actualMapping: AppSpec.Decl[];
  }): void {
    const { function: mappingFn, extraArgs = [] } = expectedMapping;
    decls.forEach((config, name) => {
      const resultDecl = getDecl(actualMapping, declType, name);

      expect(resultDecl).toStrictEqual({
        declType,
        declName: name,
        declValue: mappingFn(config, ...extraArgs),
      });
    });
  }

  /**
   * Retrieves a specific declaration from a list of declarations based on its type and name.
   */
  function getDecl<T extends keyof AppSpec.DeclTypeToValue>(
    decls: AppSpec.Decl[],
    declType: T,
    declName: string,
  ): AppSpec.GetDeclForType<T> | undefined {
    const decl = decls.find(
      (decl): decl is AppSpec.GetDeclForType<T> =>
        decl.declType === declType && decl.declName === declName,
    );

    return decl;
  }
});
