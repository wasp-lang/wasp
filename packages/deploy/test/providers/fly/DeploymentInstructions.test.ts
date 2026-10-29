import { describe, expect, test } from "vitest";
import type {
  WaspCliExe,
  WaspProjectDir,
} from "../../../src/common/brandedTypes.js";
import { createDeploymentInstructions } from "../../../src/providers/fly/DeploymentInstructions.js";

describe("createDeploymentInstructions", () => {
  test("generates correct app names from baseName", () => {
    const baseName = "my-app";
    const result = createDeploymentInstructions({
      baseName,
      cmdOptions: {
        waspExe: "wasp" as WaspCliExe,
        waspProjectDir: "/path/to/project" as WaspProjectDir,
      },
      tomlFilePaths: {
        serverTomlPath: "/path/fly-server.toml",
        clientTomlPath: "/path/fly-client.toml",
      },
    });
    expect(result.clientFlyAppName).toBe(`${baseName}-client`);
    expect(result.serverFlyAppName).toBe(`${baseName}-server`);
    expect(result.dbName).toBe(`${baseName}-db`);
  });
});
