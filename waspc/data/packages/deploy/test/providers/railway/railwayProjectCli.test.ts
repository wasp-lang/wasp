import { beforeEach, describe, expect, test, vi } from "vitest";
import type { RailwayCliExe } from "../../../src/providers/railway/brandedTypes.js";
import {
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";

const fakeRailwayExe = "/usr/bin/railway" as RailwayCliExe;

// Captures the template strings and interpolated values from each tagged template call.
let lastTemplateStrings: TemplateStringsArray | null = null;
let lastTemplateValues: unknown[] = [];
let mockStdout = "[]";

// zx's $ is used as $({ verbose: false })`command ${args}`.
// The first call ($({ options })) returns a tagged template function.
// The tagged template function is then called with template strings and values.
vi.mock("zx", () => ({
  $: vi.fn((_options: unknown) => {
    const tagFn = (
      strings: TemplateStringsArray,
      ...values: unknown[]
    ): Promise<{ stdout: string }> => {
      lastTemplateStrings = strings;
      lastTemplateValues = values;
      return Promise.resolve({ stdout: mockStdout });
    };
    return tagFn;
  }),
}));

const { getRailwayProjectByName, getRailwayProjectById } = await import(
  "../../../src/providers/railway/railwayProject/cli.js"
);

function setProjectListResponse(projects: unknown[]): void {
  mockStdout = JSON.stringify(projects);
}

describe("getRailwayProjectByName", () => {
  beforeEach(() => {
    lastTemplateStrings = null;
    lastTemplateValues = [];
    mockStdout = "[]";
  });

  test("returns the project when found", async () => {
    setProjectListResponse([cliProjectWithServices, cliProjectWithoutServices]);

    const result = await getRailwayProjectByName(
      fakeRailwayExe,
      "my-project",
    );

    expect(result).not.toBeNull();
    expect(result!.id).toBe("proj-1");
    expect(result!.name).toBe("my-project");
  });

  test("returns null when the project is not found", async () => {
    setProjectListResponse([cliProjectWithServices]);

    const result = await getRailwayProjectByName(
      fakeRailwayExe,
      "nonexistent",
    );

    expect(result).toBeNull();
  });

  test("does not pass workspace args when workspace is undefined", async () => {
    setProjectListResponse([]);

    await getRailwayProjectByName(fakeRailwayExe, "any-project");

    // The template interpolation should only contain [railwayExe, []] (no workspace args).
    expect(lastTemplateValues).toEqual([fakeRailwayExe, []]);
  });

  test("passes --workspace flag when workspace is provided", async () => {
    setProjectListResponse([]);

    await getRailwayProjectByName(fakeRailwayExe, "any-project", "my-team");

    // The template interpolation should contain the workspace args.
    expect(lastTemplateValues).toEqual([
      fakeRailwayExe,
      ["--workspace", "my-team"],
    ]);
  });

  test("filters projects from the specified workspace", async () => {
    setProjectListResponse([cliProjectWithServices]);

    const result = await getRailwayProjectByName(
      fakeRailwayExe,
      "my-project",
      "my-team",
    );

    expect(result).not.toBeNull();
    expect(result!.name).toBe("my-project");
    // Verify workspace args were passed.
    expect(lastTemplateValues).toEqual([
      fakeRailwayExe,
      ["--workspace", "my-team"],
    ]);
  });
});

describe("getRailwayProjectById", () => {
  beforeEach(() => {
    lastTemplateStrings = null;
    lastTemplateValues = [];
    mockStdout = "[]";
  });

  test("returns the project when found by id", async () => {
    setProjectListResponse([cliProjectWithServices, cliProjectWithoutServices]);

    const result = await getRailwayProjectById(fakeRailwayExe, "proj-2");

    expect(result).not.toBeNull();
    expect(result!.id).toBe("proj-2");
    expect(result!.name).toBe("empty-project");
  });

  test("returns null when no project matches the id", async () => {
    setProjectListResponse([cliProjectWithServices]);

    const result = await getRailwayProjectById(
      fakeRailwayExe,
      "nonexistent-id",
    );

    expect(result).toBeNull();
  });

  test("does not pass workspace args when workspace is undefined", async () => {
    setProjectListResponse([]);

    await getRailwayProjectById(fakeRailwayExe, "any-id");

    expect(lastTemplateValues).toEqual([fakeRailwayExe, []]);
  });

  test("passes --workspace flag when workspace is provided", async () => {
    setProjectListResponse([]);

    await getRailwayProjectById(fakeRailwayExe, "any-id", "my-team");

    expect(lastTemplateValues).toEqual([
      fakeRailwayExe,
      ["--workspace", "my-team"],
    ]);
  });
});
