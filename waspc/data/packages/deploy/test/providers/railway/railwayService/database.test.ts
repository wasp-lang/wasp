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
  test("creates the service and adds a volume to it", async () => {
    mocks.railwayCli
      .mockResolvedValueOnce(jsonResult(dbService))
      .mockResolvedValueOnce(jsonResult({}));

    await expect(
      createDatabaseService(dbServiceName, dbImage, dbVolumeMountPath, options),
    ).resolves.toEqual(dbService);

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

  test("deletes the incomplete service when adding the volume fails", async () => {
    mocks.railwayCli
      .mockResolvedValueOnce(jsonResult(dbService))
      .mockRejectedValueOnce(volumeError)
      .mockResolvedValueOnce(jsonResult({}));

    await expect(
      createDatabaseService(dbServiceName, dbImage, dbVolumeMountPath, options),
    ).rejects.toBe(volumeError);

    expect(mocks.railwayCli).toHaveBeenLastCalledWith(
      ["service", "delete", "--service", dbService.id, "--yes", "--json"],
      { verbose: false },
    );
  });

  test("reports both errors when the cleanup also fails", async () => {
    const cleanupError = new Error("Failed to delete service");
    mocks.railwayCli
      .mockResolvedValueOnce(jsonResult(dbService))
      .mockRejectedValueOnce(volumeError)
      .mockRejectedValueOnce(cleanupError);

    const failedCreation = createDatabaseService(
      dbServiceName,
      dbImage,
      dbVolumeMountPath,
      options,
    );
    await expect(failedCreation).rejects.toThrow(volumeError.message);
    await expect(failedCreation).rejects.toThrow(cleanupError.message);
    await expect(failedCreation).rejects.toThrow(dbService.id);
  });
});

describe("assertDatabaseServiceHasVolume", () => {
  test("passes when the service has the volume", async () => {
    mocks.railwayCli.mockResolvedValueOnce(
      jsonResult([
        { ...dbService, volumes: [{ mountPath: dbVolumeMountPath }] },
      ]),
    );

    await expect(
      assertDatabaseServiceHasVolume(dbService, dbVolumeMountPath, options),
    ).resolves.toBeUndefined();
  });

  test("throws when the service has no volume", async () => {
    mocks.railwayCli.mockResolvedValueOnce(
      jsonResult([{ ...dbService, volumes: [] }]),
    );

    await expect(
      assertDatabaseServiceHasVolume(dbService, dbVolumeMountPath, options),
    ).rejects.toThrow(dbService.id);
  });
});

function jsonResult(value: unknown) {
  return { json: () => value };
}
