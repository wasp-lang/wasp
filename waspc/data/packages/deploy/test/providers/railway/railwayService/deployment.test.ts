import { describe, expect, test } from "vitest";
import { RailwayCliProjectStatusSchema } from "../../../../src/providers/railway/jsonOutputSchemas.js";
import { findServiceDeploymentStatus } from "../../../../src/providers/railway/railwayService/deployment.js";
import {
  cliProjectStatusInNewFormat,
  cliProjectStatusInOldFormat,
} from "../fixtures/railwayCliProjectStatus.js";

const newFormatProjectStatus = RailwayCliProjectStatusSchema.parse(
  cliProjectStatusInNewFormat,
);
const oldFormatProjectStatus = RailwayCliProjectStatusSchema.parse(
  cliProjectStatusInOldFormat,
);

describe("findServiceDeploymentStatus", () => {
  test("finds the deployment status in new CLI output", () => {
    expect(
      findServiceDeploymentStatus(newFormatProjectStatus, "Postgres"),
    ).toBe("SUCCESS");
  });

  test("finds the deployment status in old CLI output", () => {
    expect(
      findServiceDeploymentStatus(oldFormatProjectStatus, "Postgres"),
    ).toBe("DEPLOYING");
  });

  test("returns null for a service without a deployment", () => {
    expect(
      findServiceDeploymentStatus(
        newFormatProjectStatus,
        "test-project-server",
      ),
    ).toBeNull();
  });

  test("returns null for a service that doesn't exist", () => {
    expect(
      findServiceDeploymentStatus(newFormatProjectStatus, "unknown"),
    ).toBeNull();
  });
});
