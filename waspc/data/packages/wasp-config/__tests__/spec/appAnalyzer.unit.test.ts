import { afterEach } from "node:test";
import { describe, expect, test, vi } from "vitest";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { mapApp } from "../../src/spec/mapApp.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

describe("analyzeApp", () => {
  afterEach(() => vi.clearAllMocks());

  test("should parse minimal app successfully", async () => {
    await testAnalyzeApp({
      spec: Fixtures.getApp("minimal"),
      entities: Fixtures.getEntities("minimal"),
    });
  });

  test("should parse full app successfully", async () => {
    await testAnalyzeApp({
      spec: Fixtures.getApp("full"),
      entities: Fixtures.getEntities("full"),
    });
  });

  test("should parse spec from async default export", async () => {
    const spec = Fixtures.getApp("minimal");
    const entities = Fixtures.getEntities("minimal");
    const mockMainWaspTs = "main.wasp.ts";
    vi.doMock(mockMainWaspTs, () => ({ default: Promise.resolve(spec) }));

    const result = await analyzeApp(mockMainWaspTs, entities);
    const expected = mapApp(spec, entities);

    expect(result).toMatchObject({
      status: "ok",
      value: expected,
    });
  });

  test("should return an error if the default export is not defined", async () => {
    await testAnalyzeApp({
      spec: undefined as unknown as TsAppSpec.App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  test("should return an error if the default export is not an App", async () => {
    await testAnalyzeApp({
      spec: "not an App" as unknown as TsAppSpec.App,
      entities: Fixtures.getEntities("minimal"),
      options: {
        shouldReturnError: true,
      },
    });
  });

  async function testAnalyzeApp(input: {
    spec: TsAppSpec.App;
    entities: string[];
    options?: {
      shouldReturnError: boolean;
    };
  }): Promise<void> {
    const {
      spec,
      entities,
      options: { shouldReturnError } = { shouldReturnError: false },
    } = input;
    const mockMainWaspTs = "main.wasp.ts";
    vi.doMock(mockMainWaspTs, () => ({ default: spec }));

    const result = await analyzeApp(mockMainWaspTs, entities);

    if (shouldReturnError) {
      expect(result).toMatchObject({
        status: "error",
        error: expect.anything(),
      });
    } else {
      const expected = mapApp(spec, entities);
      expect(result).toMatchObject({
        status: "ok",
        value: expected,
      });
    }
  }
});
