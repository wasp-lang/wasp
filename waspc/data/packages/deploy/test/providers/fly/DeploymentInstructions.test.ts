import { describe, expect, test } from "vitest";
import type {
  WaspCliExe,
  WaspProjectDir,
} from "../../../src/common/brandedTypes.js";
import { createDeploymentInstructions } from "../../../src/providers/fly/DeploymentInstructions.js";

describe("createDeploymentInstructions", () => {
  test("generates correct app names from baseName", () => {
    const result = createDeploymentInstructions({
      baseName: "my-app",
      cmdOptions: {
        waspExe: "wasp" as WaspCliExe,
        waspProjectDir: "/path/to/project" as WaspProjectDir,
      },
      tomlFilePaths: {
        serverTomlPath: "/path/fly-server.toml",
        clientTomlPath: "/path/fly-client.toml",
      },
    });
    expect(result.clientFlyAppName).toBe("my-app-client");
    expect(result.serverFlyAppName).toBe("my-app-server");
    expect(result.dbName).toBe("my-app-db");
  });
});
