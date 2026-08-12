import { describe, expect, test } from "vitest";
import type { RailwayProjectName } from "../../../src/providers/railway/brandedTypes.js";
import {
  createRailwayClientServiceName,
  createRailwayDbServiceName,
  createRailwayServerServiceName,
  getDbServiceNameWithFallback,
} from "../../../src/providers/railway/railwayService/nameGenerator.js";
import type { RailwayProject } from "../../../src/providers/railway/railwayProject/RailwayProject.js";

describe("createRailwayClientServiceName", () => {
  test("appends -client suffix", () => {
    const name = createRailwayClientServiceName("my-app" as RailwayProjectName);
    expect(name).toBe("my-app-client");
  });
});

describe("createRailwayServerServiceName", () => {
  test("appends -server suffix", () => {
    const name = createRailwayServerServiceName("my-app" as RailwayProjectName);
    expect(name).toBe("my-app-server");
  });
});

describe("createRailwayDbServiceName", () => {
  test("appends -db suffix to project name", () => {
    const name = createRailwayDbServiceName("my-app" as RailwayProjectName);
    expect(name).toBe("my-app-db");
  });
});

/**
 * Creates a mock Railway project for testing with the specified service names.
 */
function createMockProject(serviceNames: string[]): RailwayProject {
  return {
    id: "test-project-id",
    name: "test-project",
    services: serviceNames.map((name) => ({ id: `${name}-id`, name })),
    doesServiceExist(serviceName: string) {
      return this.services.some((s) => s.name === serviceName);
    },
  };
}

describe("getDbServiceNameWithFallback", () => {
  test("returns new name when it exists", () => {
    const project = createMockProject(["my-app-db"]);
    const name = getDbServiceNameWithFallback(
      "my-app" as RailwayProjectName,
      project,
    );
    expect(name).toBe("my-app-db");
  });

  test("falls back to Postgres when new name doesn't exist", () => {
    const project = createMockProject(["Postgres"]);
    const name = getDbServiceNameWithFallback(
      "my-app" as RailwayProjectName,
      project,
    );
    expect(name).toBe("Postgres");
  });

  test("returns new name when neither exists (new deployment)", () => {
    const project = createMockProject([]);
    const name = getDbServiceNameWithFallback(
      "my-app" as RailwayProjectName,
      project,
    );
    expect(name).toBe("my-app-db");
  });
});
