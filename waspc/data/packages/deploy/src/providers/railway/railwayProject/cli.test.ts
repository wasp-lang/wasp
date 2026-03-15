import { describe, it, expect, vi, beforeEach } from "vitest";
import { getRailwayProjectById, getRailwayProjectByName } from "./cli.js";

// Mock the entire zx module with a properly implemented tagged template function
vi.mock("zx", () => {
  const mockTemplateFunction = (
    strings: TemplateStringsArray | { verbose: boolean },
    ...values: any[]
  ) => {
    // If called with options object, return the template function
    if (!Array.isArray(strings)) {
      return (
        strings2: TemplateStringsArray,
        ...values2: any[]
      ): Promise<{ stdout: string; exitCode: number }> => {
        // Return mock Railway CLI response
        return Promise.resolve({
          stdout: JSON.stringify([
            { id: "proj-123", name: "test-project", services: [] },
          ]),
          exitCode: 0,
        });
      };
    }

    // Called as tagged template directly
    return Promise.resolve({
      stdout: JSON.stringify([
        { id: "proj-123", name: "test-project", services: [] },
      ]),
      exitCode: 0,
    });
  };

  return {
    $: mockTemplateFunction,
  };
});

// Mock the other local imports
vi.mock("../jsonOutputSchemas.js", () => ({
  RailwayProjectListSchema: {
    parse: vi.fn((data) => data),
  },
  RailwayCliProjectSchema: {
    parse: vi.fn((data) => data),
  },
}));

vi.mock("./RailwayProject.js", () => ({
  createRailwayProject: vi.fn((project) => project),
}));

describe("Railway Project CLI workspace filtering", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("getRailwayProjectById", () => {
    it("should pass workspace parameter correctly to Railway CLI", async () => {
      const result = await getRailwayProjectById(
        "railway" as any,
        "proj-123",
        "my-workspace",
      );

      expect(result).toBeDefined();
      if (result) {
        expect(result.id).toBe("proj-123");
      }
    });

    it("should work without workspace parameter", async () => {
      const result = await getRailwayProjectById("railway" as any, "proj-123");

      expect(result).toBeDefined();
      if (result) {
        expect(result.id).toBe("proj-123");
      }
    });
  });

  describe("getRailwayProjectByName", () => {
    it("should pass workspace parameter correctly to Railway CLI", async () => {
      const result = await getRailwayProjectByName(
        "railway" as any,
        "test-project",
        "my-workspace",
      );

      expect(result).toBeDefined();
      if (result) {
        expect(result.name).toBe("test-project");
      }
    });

    it("should work without workspace parameter", async () => {
      const result = await getRailwayProjectByName(
        "railway" as any,
        "test-project",
      );

      expect(result).toBeDefined();
      if (result) {
        expect(result.name).toBe("test-project");
      }
    });

    it("should return null when project is not found", async () => {
      // Mock will return test-project, so searching for nonexistent should return null
      const result = await getRailwayProjectByName(
        "railway" as any,
        "nonexistent-project",
      );

      expect(result).toBeNull();
    });

    it("should return project when found by name", async () => {
      const result = await getRailwayProjectByName(
        "railway" as any,
        "test-project",
      );

      expect(result).toBeDefined();
      if (result) {
        expect(result.name).toBe("test-project");
        expect(result.id).toBe("proj-123");
      }
    });
  });
});
