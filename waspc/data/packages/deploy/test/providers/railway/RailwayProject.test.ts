import { describe, expect, test } from "vitest";
import {
  createDraftRailwayProject,
  createLinkedRailwayProject,
  createUnlinkedRailwayProject,
} from "../../../src/providers/railway/railwayProject/RailwayProject.js";
import type {
  DraftRailwayProject,
  LinkedRailwayProject,
  RailwayProject,
  UnlinkedRailwayProject,
} from "../../../src/providers/railway/railwayProject/RailwayProject.js";
import {
  cliProjectWithServices,
  cliProjectWithoutServices,
} from "./fixtures/railwayCliProject.js";

describe("createLinkedRailwayProject", () => {
  test("extracts project id and name", () => {
    const project = createLinkedRailwayProject(cliProjectWithServices);
    expect(project.id).toBe("proj-1");
    expect(project.name).toBe("my-project");
  });

  test("sets status to linked", () => {
    const project = createLinkedRailwayProject(cliProjectWithServices);
    expect(project.status).toBe("linked");
  });

  test("flattens edge-node graph into services array", () => {
    const project = createLinkedRailwayProject(cliProjectWithServices);
    expect(project.services).toEqual([
      { id: "svc-1", name: "web-server" },
      { id: "svc-2", name: "Postgres" },
    ]);
  });

  test("doesServiceExist returns true for existing service", () => {
    const project = createLinkedRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Postgres")).toBe(true);
  });

  test("doesServiceExist returns false for missing service", () => {
    const project = createLinkedRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Redis")).toBe(false);
  });

  test("handles project with no services", () => {
    const project = createLinkedRailwayProject(cliProjectWithoutServices);
    expect(project.services).toEqual([]);
    expect(project.doesServiceExist("anything")).toBe(false);
  });
});

describe("createUnlinkedRailwayProject", () => {
  test("extracts project id and name", () => {
    const project = createUnlinkedRailwayProject(cliProjectWithServices);
    expect(project.id).toBe("proj-1");
    expect(project.name).toBe("my-project");
  });

  test("sets status to unlinked", () => {
    const project = createUnlinkedRailwayProject(cliProjectWithServices);
    expect(project.status).toBe("unlinked");
  });

  test("flattens edge-node graph into services array", () => {
    const project = createUnlinkedRailwayProject(cliProjectWithServices);
    expect(project.services).toEqual([
      { id: "svc-1", name: "web-server" },
      { id: "svc-2", name: "Postgres" },
    ]);
  });

  test("doesServiceExist returns true for existing service", () => {
    const project = createUnlinkedRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Postgres")).toBe(true);
  });

  test("doesServiceExist returns false for missing service", () => {
    const project = createUnlinkedRailwayProject(cliProjectWithServices);
    expect(project.doesServiceExist("Redis")).toBe(false);
  });
});

describe("createDraftRailwayProject", () => {
  test("sets status to draft", () => {
    const project = createDraftRailwayProject();
    expect(project.status).toBe("draft");
  });

  test("has no project data properties", () => {
    const project = createDraftRailwayProject();
    expect(Object.keys(project)).toEqual(["status"]);
  });
});

describe("RailwayProject discriminated union", () => {
  test("narrows to LinkedRailwayProject on status linked", () => {
    const project: RailwayProject = createLinkedRailwayProject(
      cliProjectWithServices,
    );
    if (project.status === "linked") {
      // TypeScript should narrow to LinkedRailwayProject here.
      const linked: LinkedRailwayProject = project;
      expect(linked.id).toBe("proj-1");
      expect(linked.doesServiceExist("web-server")).toBe(true);
    } else {
      throw new Error("Expected status to be linked");
    }
  });

  test("narrows to UnlinkedRailwayProject on status unlinked", () => {
    const project: RailwayProject = createUnlinkedRailwayProject(
      cliProjectWithServices,
    );
    if (project.status === "unlinked") {
      const unlinked: UnlinkedRailwayProject = project;
      expect(unlinked.id).toBe("proj-1");
      expect(unlinked.name).toBe("my-project");
    } else {
      throw new Error("Expected status to be unlinked");
    }
  });

  test("narrows to DraftRailwayProject on status draft", () => {
    const project: RailwayProject = createDraftRailwayProject();
    if (project.status === "draft") {
      const draft: DraftRailwayProject = project;
      expect(draft.status).toBe("draft");
      // Draft projects should not have id, name, services etc.
      expect("id" in draft).toBe(false);
    } else {
      throw new Error("Expected status to be draft");
    }
  });

  test("exhaustive switch handles all variants", () => {
    const projects: RailwayProject[] = [
      createLinkedRailwayProject(cliProjectWithServices),
      createUnlinkedRailwayProject(cliProjectWithServices),
      createDraftRailwayProject(),
    ];

    const statuses = projects.map((project) => {
      switch (project.status) {
        case "linked":
          return "linked";
        case "unlinked":
          return "unlinked";
        case "draft":
          return "draft";
        default:
          project satisfies never;
          throw new Error("Unhandled status");
      }
    });

    expect(statuses).toEqual(["linked", "unlinked", "draft"]);
  });
});
