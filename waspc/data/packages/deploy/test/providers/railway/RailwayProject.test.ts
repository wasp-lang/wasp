import { describe, expect, test } from "vitest";
import { createRailwayProject } from "../../../src/providers/railway/railwayProject/RailwayProject.js";
import {
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";

describe("createRailwayProject", () => {
  test("extracts project id and name", () => {
    const project = createRailwayProject(cliProjectWithServices);
    expect(project.id).toBe("proj-1");
    expect(project.name).toBe("my-project");
  });

  test("flattens edge-node graph into services array", () => {
    const project = createRailwayProject(cliProjectWithServices);
    expect(project.services).toEqual([
      { id: "svc-1", name: "web-server" },
      { id: "svc-2", name: "Postgres" },
    ]);
  });

  test("doesServiceExist returns true for existing service", () => {
    const project = createRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Postgres")).toBe(true);
  });

  test("doesServiceExist returns false for missing service", () => {
    const project = createRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Redis")).toBe(false);
  });

  test("handles project with no services", () => {
    const project = createRailwayProject(cliProjectWithoutServices);
    expect(project.services).toEqual([]);
    expect(project.doesServiceExist("anything")).toBe(false);
  });
});
