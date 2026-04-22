import { afterEach } from "node:test";
import { describe, expect, test, vi } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { mapApp } from "../../src/spec/mapApp.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("analyzeApp", () => {
  afterEach(() => vi.clearAllMocks());

  test("should parse an app successfully", async () => {
    await testAnalyzeApp({
      app: Fixtures.getMinimalApp(),
      entities: Fixtures.getEntities("minimal"),
    });
  });

  test("should parse app from async default export", async () => {
    const app = Fixtures.getMinimalApp();
    const entities = Fixtures.getEntities("minimal");
    const mockMainWaspTs = "main.wasp.ts";
    vi.doMock(mockMainWaspTs, () => ({ default: Promise.resolve(app) }));

    const result = await analyzeApp(mockMainWaspTs, entities);
    const expected = mapApp(app, entities);

    expect(result).toMatchObject({
      status: "ok",
      value: expected,
    });
  });

  test("should return an error if the default export is not defined", async () => {
    await testAnalyzeApp({
      app: undefined as unknown as TsAppSpec.App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  test("should return an error if the default export is not an App", async () => {
    await testAnalyzeApp({
      app: "not an App" as unknown as TsAppSpec.App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  async function testAnalyzeApp(input: {
    app: TsAppSpec.App;
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
      const expected = mapApp(app, entities);
      expect(result).toMatchObject({
        status: "ok",
        value: expected,
      });
    }
  }
});
