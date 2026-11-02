import { describe, expect, test } from "vitest";
import type {
  WaspCliExe,
  WaspProjectDir,
} from "../../../src/common/brandedTypes.js";
import { createDeploymentInstructions } from "../../../src/providers/railway/DeploymentInstructions.js";
import type {
  RailwayCliExe,
  RailwayProjectName,
} from "../../../src/providers/railway/brandedTypes.js";

describe("createDeploymentInstructions", () => {
  test("generates correct service names from project name", () => {
    const baseName = "my-app";
    const result = createDeploymentInstructions(
      baseName as RailwayProjectName,
      {
        waspExe: "wasp" as WaspCliExe,
        railwayExe: "railway" as RailwayCliExe,
        waspProjectDir: "/path/to/project" as WaspProjectDir,
      },
    );
    expect(result.clientServiceName).toBe(`${baseName}-client`);
    expect(result.serverServiceName).toBe(`${baseName}-server`);
    expect(result.dbServiceName).toBe("Postgres");
  });
});
