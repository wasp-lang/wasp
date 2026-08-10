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

import {
  assertDatabaseServiceHasVolume,
  createDatabaseServiceWithVolume,
} from "../../../../src/providers/railway/railwayService/database.js";

const databaseVolumeMountPath = "/var/lib/postgresql/data";
const dbServiceName = "Postgres" as DbServiceName;
const dbService = { id: "service-id", name: dbServiceName };
const options = {
  railwayExe: "railway" as RailwayCliExe,
  waspProjectDir: "/app" as WaspProjectDir,
};
const volumeError = new Error("Failed to create volume");

beforeEach(() => {
  mocks.railwayCli.mockReset();
});

describe("createDatabaseServiceWithVolume", () => {
  test("removes the newly created service when volume creation fails", async () => {
    mockFailedVolumeCreation();
    mocks.railwayCli.mockResolvedValueOnce(jsonResult(dbService));

    await expect(createDatabase()).rejects.toBe(volumeError);

    expect(mocks.railwayCli).toHaveBeenNthCalledWith(
      2,
      [
        "volume",
        "--service",
        dbService.id,
        "add",
        "--mount-path",
        databaseVolumeMountPath,
        "--json",
      ],
      { verbose: false },
    );
    expect(mocks.railwayCli).toHaveBeenLastCalledWith(
      ["service", "delete", "--service", dbService.id, "--yes", "--json"],
      { verbose: false },
    );
  });

  test("reports the service ID when volume creation and cleanup both fail", async () => {
    const rollbackError = new Error("Failed to delete service");
    mockFailedVolumeCreation();
    mocks.railwayCli.mockRejectedValueOnce(rollbackError);

    await expect(createDatabase()).rejects.toThrow(dbService.id);
  });

  test("continues when Railway created the volume before returning an error", async () => {
    mockFailedVolumeCreation([databaseVolumeMountPath]);

    await expect(createDatabase()).resolves.toBeUndefined();
    expect(mocks.railwayCli).toHaveBeenCalledTimes(3);
  });
});

describe("assertDatabaseServiceHasVolume", () => {
  test("rejects the exact service when only another service has the volume", async () => {
    const otherService = { id: "other-service-id", name: dbServiceName };
    mocks.railwayCli.mockResolvedValueOnce(
      jsonResult([
        withVolumes(otherService, [databaseVolumeMountPath]),
        withVolumes(dbService),
      ]),
    );

    await expect(
      assertDatabaseServiceHasVolume(dbService, options),
    ).rejects.toThrow(dbService.id);
  });
});

function createDatabase(): Promise<void> {
  return createDatabaseServiceWithVolume(
    dbServiceName,
    "postgres-image",
    options,
  );
}

function mockFailedVolumeCreation(mountPaths: string[] = []): void {
  mocks.railwayCli
    .mockResolvedValueOnce(jsonResult(dbService))
    .mockRejectedValueOnce(volumeError)
    .mockResolvedValueOnce(jsonResult([withVolumes(dbService, mountPaths)]));
}

function withVolumes(service: typeof dbService, mountPaths: string[] = []) {
  return {
    ...service,
    volumes: mountPaths.map((mountPath) => ({ mountPath })),
  };
}

function jsonResult(value: unknown) {
  return { stdout: JSON.stringify(value) };
}
