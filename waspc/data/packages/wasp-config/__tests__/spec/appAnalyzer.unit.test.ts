import { afterEach, describe, expect, test, vi } from "vitest";
import { loadWaspTsFileDefaultExport } from "../../src/spec-pipeline/index.js";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { mapApp } from "../../src/spec/mapApp.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import * as Fixtures from "./testFixtures.js";

vi.mock("../../src/spec-pipeline/index.js", () => ({
  loadWaspTsFileDefaultExport: vi.fn(),
}));

const mockLoadWaspTsFileDefaultExport = vi.mocked(loadWaspTsFileDefaultExport);

describe("analyzeApp", () => {
  afterEach(() => vi.clearAllMocks());

  test("should load and parse an app successfully", async () => {
    await testAnalyzeApp({
      app: Fixtures.getApp("minimal"),
      entities: Fixtures.getEntities("minimal"),
    });
  });

  test("should load and parse full app successfully", async () => {
    await testAnalyzeApp({
      app: Fixtures.getApp("full"),
      entities: Fixtures.getEntities("full"),
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
    mockLoadWaspTsFileDefaultExport.mockResolvedValue(app);

    const result = await analyzeApp({
      waspTsSpecPath: "main.wasp.ts",
      tsconfigPath: "tsconfig.wasp.json",
      entityNames: entities,
    });

    expect(mockLoadWaspTsFileDefaultExport).toHaveBeenCalledWith({
      inputPath: "main.wasp.ts",
      tsconfigPath: "tsconfig.wasp.json",
    });

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
