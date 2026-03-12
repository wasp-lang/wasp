import { describe, expect, test } from "vitest";
import { createRailwayProject } from "../../../src/providers/railway/railwayProject/RailwayProject.js";

describe("createRailwayProject", () => {
  const cliProject = {
    id: "proj-1",
    name: "my-project",
    services: {
      edges: [
        { node: { id: "svc-1", name: "web-server" } },
        { node: { id: "svc-2", name: "Postgres" } },
      ],
    },
  };

  test("extracts project id and name", () => {
    const project = createRailwayProject(cliProject);
    expect(project.id).toBe("proj-1");
    expect(project.name).toBe("my-project");
  });

  test("flattens edge-node graph into services array", () => {
    const project = createRailwayProject(cliProject);
    expect(project.services).toEqual([
      { id: "svc-1", name: "web-server" },
      { id: "svc-2", name: "Postgres" },
    ]);
  });

  test("doesServiceExist returns true for existing service", () => {
    const project = createRailwayProject(cliProject);
    expect(project.doesServiceExist("Postgres")).toBe(true);
  });

  test("doesServiceExist returns false for missing service", () => {
    const project = createRailwayProject(cliProject);
    expect(project.doesServiceExist("Redis")).toBe(false);
  });

  test("handles project with no services", () => {
    const emptyProject = {
      id: "proj-2",
      name: "empty",
      services: { edges: [] },
    };
    const project = createRailwayProject(emptyProject);
    expect(project.services).toEqual([]);
    expect(project.doesServiceExist("anything")).toBe(false);
  });
});
