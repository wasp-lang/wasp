import { describe, expect, test } from "vitest";
import { RailwayCliProjectStatusSchema } from "../../../../src/providers/railway/jsonOutputSchemas.js";
import { findServiceDeploymentStatus } from "../../../../src/providers/railway/railwayService/deployment.js";
import { cliProjectStatus } from "../fixtures/railwayCliProjectStatus.js";

const projectStatus = RailwayCliProjectStatusSchema.parse(cliProjectStatus);

describe("findServiceDeploymentStatus", () => {
  test("finds the deployment status", () => {
    expect(findServiceDeploymentStatus(projectStatus, "Postgres")).toBe(
      "SUCCESS",
    );
  });

  test("returns null for a service without a deployment", () => {
    expect(
      findServiceDeploymentStatus(projectStatus, "test-project-server"),
    ).toBeNull();
  });

  test("returns null for a service that doesn't exist", () => {
    expect(findServiceDeploymentStatus(projectStatus, "unknown")).toBeNull();
  });
});
