import { describe, it, expect, vi, beforeEach } from "vitest";
import { getRailwayProjectStatus, ProjectStatus } from "./index.js";

// Mock the cli module
vi.mock("./cli.js", () => ({
  getRailwayProjectById: vi.fn(),
  getRailwayProjectByName: vi.fn(),
  getRailwayProjectForDirectory: vi.fn(),
}));

describe("Railway Project Status workspace filtering", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("getRailwayProjectStatus", () => {
    it("should pass workspace parameter when checking for existing project by ID", async () => {
      const {
        getRailwayProjectById,
        getRailwayProjectForDirectory,
      } = await import("./cli.js");

      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(null);

      const mockProject = {
        id: "proj-123",
        name: "test-project",
        services: [],
        doesServiceExist: vi.fn(),
      };
      vi.mocked(getRailwayProjectById).mockResolvedValue(mockProject as any);

      await getRailwayProjectStatus({
        projectName: "test-project" as any,
        waspProjectDir: "/test" as any,
        railwayExe: "railway" as any,
        existingProjectId: "proj-123" as any,
        workspace: "my-workspace",
      });

      expect(getRailwayProjectById).toHaveBeenCalledWith(
        "railway",
        "proj-123",
        "my-workspace",
      );
    });

    it("should pass workspace parameter when checking for unique project name", async () => {
      const {
        getRailwayProjectByName,
        getRailwayProjectForDirectory,
      } = await import("./cli.js");

      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(null);
      vi.mocked(getRailwayProjectByName).mockResolvedValue(null);

      const result = await getRailwayProjectStatus({
        projectName: "test-project" as any,
        waspProjectDir: "/test" as any,
        railwayExe: "railway" as any,
        workspace: "my-workspace",
      });

      expect(getRailwayProjectByName).toHaveBeenCalledWith(
        "railway",
        "test-project",
        "my-workspace",
      );

      expect(result.status).toBe(ProjectStatus.MISSING_PROJECT);
    });

    it("should work without workspace parameter for backward compatibility", async () => {
      const {
        getRailwayProjectByName,
        getRailwayProjectForDirectory,
      } = await import("./cli.js");

      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(null);
      vi.mocked(getRailwayProjectByName).mockResolvedValue(null);

      await getRailwayProjectStatus({
        projectName: "test-project" as any,
        waspProjectDir: "/test" as any,
        railwayExe: "railway" as any,
      });

      expect(getRailwayProjectByName).toHaveBeenCalledWith(
        "railway",
        "test-project",
        undefined,
      );
    });

    it("should return EXISTING_PROJECT_ALREADY_LINKED when project is already linked", async () => {
      const { getRailwayProjectForDirectory } = await import("./cli.js");

      const mockProject = {
        id: "proj-123",
        name: "test-project",
        services: [],
        doesServiceExist: vi.fn(),
      };
      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(
        mockProject as any,
      );

      const result = await getRailwayProjectStatus({
        projectName: "test-project" as any,
        waspProjectDir: "/test" as any,
        railwayExe: "railway" as any,
      });

      expect(result.status).toBe(ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED);
      expect(result.project).toBe(mockProject);
    });

    it("should throw error when existing project ID does not exist in specified workspace", async () => {
      const {
        getRailwayProjectById,
        getRailwayProjectForDirectory,
      } = await import("./cli.js");

      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(null);
      vi.mocked(getRailwayProjectById).mockResolvedValue(null);

      await expect(
        getRailwayProjectStatus({
          projectName: "test-project" as any,
          waspProjectDir: "/test" as any,
          railwayExe: "railway" as any,
          existingProjectId: "proj-999" as any,
          workspace: "my-workspace",
        }),
      ).rejects.toThrow('Project with ID "proj-999" does not exist.');
    });

    it("should throw error when project with same name already exists in workspace", async () => {
      const {
        getRailwayProjectByName,
        getRailwayProjectForDirectory,
      } = await import("./cli.js");

      vi.mocked(getRailwayProjectForDirectory).mockResolvedValue(null);

      const mockExistingProject = {
        id: "proj-456",
        name: "test-project",
        services: [],
      };
      vi.mocked(getRailwayProjectByName).mockResolvedValue(
        mockExistingProject as any,
      );

      await expect(
        getRailwayProjectStatus({
          projectName: "test-project" as any,
          waspProjectDir: "/test" as any,
          railwayExe: "railway" as any,
          workspace: "my-workspace",
        }),
      ).rejects.toThrow(/Project with name "test-project" already exists/);
    });
  });
});
