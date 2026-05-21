import { afterEach, describe, expect, test, vi } from "vitest";
import { loadWaspTsSpecDefaultExport } from "../../src/spec-pipeline/loadWaspTsSpec.js";
import { analyzeApp } from "../../src/spec/appAnalyzer.js";
import { mapApp } from "../../src/spec/mapApp.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import * as Fixtures from "./testFixtures.js";

vi.mock("../../src/spec-pipeline/loadWaspTsSpec.js", () => ({
  loadWaspTsSpecDefaultExport: vi.fn(),
}));

const mockLoadWaspTsSpecDefaultExport = vi.mocked(loadWaspTsSpecDefaultExport);

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
    mockLoadWaspTsSpecDefaultExport.mockResolvedValue(app);

    const analyze = () =>
      analyzeApp({
        waspTsSpecPath: "main.wasp.ts",
        tsconfigPath: "tsconfig.wasp.json",
        projectRootDir: "/project",
        entityNames: entities,
      });

    if (shouldReturnError) {
      await expect(analyze()).rejects.toThrowError(SpecUserError);
    } else {
      const result = await analyze();
      const expected = mapApp(app, entities);

      expect(result).toEqual(expected);
    }

    expect(mockLoadWaspTsSpecDefaultExport).toHaveBeenCalledWith({
      specPath: "main.wasp.ts",
      tsconfigPath: "tsconfig.wasp.json",
      projectRootDir: "/project",
    });
  }
});
