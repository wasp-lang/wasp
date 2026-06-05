import { describe, expect, test } from "vitest";
import { createRailwayProject } from "../../../src/providers/railway/railwayProject/RailwayProject.js";
import {
  cliProjectInDifferentWorkspace,
  cliProjectSameNameDifferentWorkspace,
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";

/**
 * These tests exercise the workspace filtering logic that
 * getRailwayProjects() applies client-side after fetching
 * the full project list from `railway list --json`.
 *
 * The filtering is: match projects where workspace.name OR workspace.id
 * equals the provided --workspace value.
 */
describe("workspace filtering", () => {
  const allFixtures = [
    cliProjectWithServices, // ws-1 / my-workspace, name "my-project"
    cliProjectWithoutServices, // ws-1 / my-workspace, name "empty-project"
    cliProjectInDifferentWorkspace, // ws-2 / other-workspace, name "other-project"
    cliProjectSameNameDifferentWorkspace, // ws-2 / other-workspace, name "my-project"
  ];

  const allProjects = allFixtures.map(createRailwayProject);

  function filterByWorkspace(
    projects: ReturnType<typeof createRailwayProject>[],
    workspace?: string,
  ) {
    if (!workspace) return projects;
    return projects.filter(
      (p) => p.workspace.name === workspace || p.workspace.id === workspace,
    );
  }

  test("returns all projects when no workspace filter is provided", () => {
    const result = filterByWorkspace(allProjects);
    expect(result).toHaveLength(4);
  });

  test("filters by workspace name", () => {
    const result = filterByWorkspace(allProjects, "my-workspace");
    expect(result).toHaveLength(2);
    expect(result.every((p) => p.workspace.name === "my-workspace")).toBe(true);
  });

  test("filters by workspace id", () => {
    const result = filterByWorkspace(allProjects, "ws-2");
    expect(result).toHaveLength(2);
    expect(result.every((p) => p.workspace.id === "ws-2")).toBe(true);
  });

  test("returns empty when workspace does not match any project", () => {
    const result = filterByWorkspace(allProjects, "nonexistent-workspace");
    expect(result).toHaveLength(0);
  });

  describe("regression: same project name in different workspaces", () => {
    test("my-project in my-workspace is distinct from my-project in other-workspace", () => {
      const myWorkspaceProjects = filterByWorkspace(
        allProjects,
        "my-workspace",
      );
      const otherWorkspaceProjects = filterByWorkspace(
        allProjects,
        "other-workspace",
      );

      const myWorkspaceMyProject = myWorkspaceProjects.find(
        (p) => p.name === "my-project",
      );
      const otherWorkspaceMyProject = otherWorkspaceProjects.find(
        (p) => p.name === "my-project",
      );

      expect(myWorkspaceMyProject).toBeDefined();
      expect(otherWorkspaceMyProject).toBeDefined();
      expect(myWorkspaceMyProject!.id).toBe("proj-1");
      expect(otherWorkspaceMyProject!.id).toBe("proj-4");
    });

    test("filtering by workspace id also correctly disambiguates same-name projects", () => {
      const ws1Projects = filterByWorkspace(allProjects, "ws-1");
      const ws2Projects = filterByWorkspace(allProjects, "ws-2");

      const ws1MyProject = ws1Projects.find((p) => p.name === "my-project");
      const ws2MyProject = ws2Projects.find((p) => p.name === "my-project");

      expect(ws1MyProject!.id).toBe("proj-1");
      expect(ws2MyProject!.id).toBe("proj-4");
    });
  });
});
