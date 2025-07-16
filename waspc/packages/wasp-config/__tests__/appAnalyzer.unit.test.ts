import { afterEach } from "node:test";
import { describe, expect, test, vi } from "vitest";
import { GET_TS_APP_SPEC } from "../src/_private.js";
import { analyzeApp } from "../src/appAnalyzer.js";
import { mapTsAppSpecToAppSpecDecls } from "../src/mapTsAppSpecToAppSpecDecls.js";
import { App } from "../src/publicApi/App.js";
import * as Fixtures from "./testFixtures.js";

describe("analyzeApp", () => {
  afterEach(() => vi.clearAllMocks());

  test("should parse minimal app sucessfully", async () => {
    await testAnalyzeApp({
      app: Fixtures.createApp("minimal").app,
      entities: Fixtures.getEntities("minimal"),
    });
  });

  test("should parse full app sucessfully", async () => {
    await testAnalyzeApp({
      app: Fixtures.createApp("full").app,
      entities: Fixtures.getEntities("full"),
    });
  });

  test("should return an error if the default export is not defined", async () => {
    await testAnalyzeApp({
      app: undefined as unknown as App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  test("should return an error if the default export is not an instance of App", async () => {
    await testAnalyzeApp({
      app: "not an instance of App" as unknown as App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  async function testAnalyzeApp(input: {
    app: App;
    entities: string[];
    options?: {
      shouldReturnError: boolean;
    };
  }): Promise<void> {
    const {
      app,
      entities,
      options: { shouldReturnError } = { shouldReturnError: false },
    } = input;
    const mockMainWaspTs = "main.wasp.ts";
    vi.doMock(mockMainWaspTs, () => ({ default: app }));

    const result = await analyzeApp(mockMainWaspTs, entities);

    if (shouldReturnError) {
      expect(result).toMatchObject({
        status: "error",
        error: expect.anything(),
      });
    } else {
      const tsAppSpec = app[GET_TS_APP_SPEC]();
      const appSpecDecls = mapTsAppSpecToAppSpecDecls(tsAppSpec, entities);

      expect(result).toMatchObject({
        status: "ok",
        value: appSpecDecls,
      });
    }
  }
});
