import { beforeEach, describe, expect, test, vi } from "vitest";
import { WaspProjectDir } from "../../../../src/common/brandedTypes.js";
import {
  DbServiceName,
  RailwayCliExe,
} from "../../../../src/providers/railway/brandedTypes.js";

const mocks = vi.hoisted(() => ({
  railwayCli: vi.fn(),
}));

vi.mock("../../../../src/common/zx.js", () => ({
  createCommandWithCwd: () => mocks.railwayCli,
}));

vi.mock("../../../../src/common/terminal.js", () => ({
  waspSays: vi.fn(),
}));

import { RailwayCliService } from "../../../../src/providers/railway/jsonOutputSchemas.js";
import {
  assertDatabaseServiceHasVolume,
  createDatabaseService,
} from "../../../../src/providers/railway/railwayService/database.js";

const dbVolumeMountPath = "/var/lib/postgresql/data";
const dbServiceName = "Postgres" as DbServiceName;
const dbImage = "postgres-image";
const dbService = { id: "service-id", name: dbServiceName };
const options = {
  railwayExe: "railway" as RailwayCliExe,
  waspProjectDir: "/app" as WaspProjectDir,
};
const volumeError = new Error("Failed to create volume");

beforeEach(() => {
  mocks.railwayCli.mockReset();
});

describe("createDatabaseService", () => {
  test("creates the service with Postgres variables and adds a volume", async () => {
    mocks.railwayCli
      .mockResolvedValueOnce(jsonResult(dbService))
      .mockResolvedValueOnce(jsonResult({}));

    await expect(createDatabase()).resolves.toEqual(dbService);

    expect(mocks.railwayCli).toHaveBeenCalledTimes(2);
    expect(mocks.railwayCli).toHaveBeenNthCalledWith(
      1,
      [
        "add",
        "--service",
        dbServiceName,
        "--image",
        dbImage,
        "--variables",
        "POSTGRES_DB=railway",
        "--variables",
        "POSTGRES_USER=postgres",
        "--variables",
        "POSTGRES_PASSWORD=${{secret()}}",
        "--variables",
        "PORT=5432",
        "--variables",
        `PGDATA=${dbVolumeMountPath}/pgdata`,
        "--variables",
        "DATABASE_URL=postgresql://${{POSTGRES_USER}}:${{POSTGRES_PASSWORD}}@${{RAILWAY_PRIVATE_DOMAIN}}:${{PORT}}/${{POSTGRES_DB}}",
        "--json",
      ],
      { verbose: false },
    );
    expect(mocks.railwayCli).toHaveBeenNthCalledWith(
      2,
      [
        "volume",
        "--service",
        dbService.id,
        "add",
        "--mount-path",
        dbVolumeMountPath,
        "--json",
      ],
      { verbose: false },
    );
  });

  test("removes the newly created service when volume creation fails", async () => {
    mockServiceCreatedThenVolumeFailed();
    mocks.railwayCli.mockResolvedValueOnce(jsonResult(dbService));

    await expect(createDatabase()).rejects.toBe(volumeError);

    expect(mocks.railwayCli).toHaveBeenLastCalledWith(
      ["service", "delete", "--service", dbService.id, "--yes", "--json"],
      { verbose: false },
    );
  });

  test("reports both errors and the service ID when volume creation and cleanup both fail", async () => {
    const cleanupError = new Error("Failed to delete service");
    mockServiceCreatedThenVolumeFailed();
    mocks.railwayCli.mockRejectedValueOnce(cleanupError);

    const failedCreation = createDatabase();
    await expect(failedCreation).rejects.toThrow(dbService.id);
    await expect(failedCreation).rejects.toThrow(volumeError.message);
    await expect(failedCreation).rejects.toThrow(cleanupError.message);
  });
});

describe("assertDatabaseServiceHasVolume", () => {
  test("passes when the service has the required volume", async () => {
    mocks.railwayCli.mockResolvedValueOnce(
      jsonResult([withVolumes(dbService, [dbVolumeMountPath])]),
    );

    await expect(
      assertDatabaseServiceHasVolume(dbService, options),
    ).resolves.toBeUndefined();
  });

  test("rejects the exact service when only another service has the volume", async () => {
    const otherService = { id: "other-service-id", name: dbServiceName };
    mocks.railwayCli.mockResolvedValueOnce(
      jsonResult([
        withVolumes(otherService, [dbVolumeMountPath]),
        withVolumes(dbService, []),
      ]),
    );

    await expect(
      assertDatabaseServiceHasVolume(dbService, options),
    ).rejects.toThrow(dbService.id);
  });
});

function createDatabase() {
  return createDatabaseService(dbServiceName, dbImage, options);
}

function mockServiceCreatedThenVolumeFailed(): void {
  mocks.railwayCli
    .mockResolvedValueOnce(jsonResult(dbService))
    .mockRejectedValueOnce(volumeError);
}

function withVolumes(service: RailwayCliService, mountPaths: string[]) {
  return {
    ...service,
    volumes: mountPaths.map((mountPath) => ({ mountPath })),
  };
}

function jsonResult(value: unknown) {
  return {
    stdout: JSON.stringify(value),
    exitCode: 0,
    json: () => value,
  };
}
