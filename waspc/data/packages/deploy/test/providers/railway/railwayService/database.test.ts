import { beforeEach, describe, expect, test, vi } from "vitest";
import { WaspProjectDir } from "../../../../src/common/brandedTypes.js";
import {
  DbServiceName,
  RailwayCliExe,
} from "../../../../src/providers/railway/brandedTypes.js";
import {
  RailwayCliServiceListSchema,
  RailwayCliServiceSchema,
} from "../../../../src/providers/railway/jsonOutputSchemas.js";

const mocks = vi.hoisted(() => ({
  railwayCli: vi.fn(),
  runJsonCommand: vi.fn(),
}));

vi.mock("../../../../src/common/zx.js", () => ({
  createCommandWithCwd: () => mocks.railwayCli,
  runJsonCommand: mocks.runJsonCommand,
}));

vi.mock("../../../../src/common/terminal.js", () => ({
  waspSays: vi.fn(),
}));

import {
  assertDatabaseServiceHasVolume,
  createDatabaseService,
} from "../../../../src/providers/railway/railwayService/database.js";

const dbVolumeMountPath = "/custom/postgresql/data";
const dbServiceName = "Postgres" as DbServiceName;
const dbImage = "postgres-image";
const dbService = { id: "service-id", name: dbServiceName };
const options = {
  railwayExe: "railway" as RailwayCliExe,
  waspProjectDir: "/app" as WaspProjectDir,
};
const createDatabaseServiceParams = {
  serviceName: dbServiceName,
  imageSpec: {
    image: dbImage,
    volumeMountPath: dbVolumeMountPath,
  },
  ...options,
};
const volumeError = new Error("Failed to create volume");

beforeEach(() => {
  mocks.railwayCli.mockReset();
  mocks.runJsonCommand.mockReset();
});

describe("createDatabaseService", () => {
  test("uses the configured mount path for PGDATA and the volume", async () => {
    mocks.runJsonCommand.mockResolvedValueOnce(dbService);
    mocks.railwayCli.mockResolvedValueOnce({});

    await expect(
      createDatabaseService(createDatabaseServiceParams),
    ).resolves.toEqual(dbService);

    expect(mocks.runJsonCommand).toHaveBeenCalledExactlyOnceWith(
      mocks.railwayCli,
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
      RailwayCliServiceSchema,
    );
    expect(mocks.railwayCli).toHaveBeenCalledExactlyOnceWith(
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
    mocks.runJsonCommand.mockResolvedValueOnce(dbService);
    mocks.railwayCli
      .mockRejectedValueOnce(volumeError)
      .mockResolvedValueOnce({});

    await expect(
      createDatabaseService(createDatabaseServiceParams),
    ).rejects.toBe(volumeError);

    expect(mocks.railwayCli).toHaveBeenLastCalledWith(
      ["service", "delete", "--service", dbService.id, "--yes", "--json"],
      { verbose: false },
    );
  });

  test("reports both errors when the cleanup also fails", async () => {
    const cleanupError = new Error("Failed to delete service");
    mocks.runJsonCommand.mockResolvedValueOnce(dbService);
    mocks.railwayCli
      .mockRejectedValueOnce(volumeError)
      .mockRejectedValueOnce(cleanupError);

    const failedCreation = createDatabaseService(createDatabaseServiceParams);
    await expect(failedCreation).rejects.toThrow(volumeError.message);
    await expect(failedCreation).rejects.toThrow(cleanupError.message);
    await expect(failedCreation).rejects.toThrow(dbService.id);
  });
});

describe("assertDatabaseServiceHasVolume", () => {
  test("passes when the service has the volume", async () => {
    mocks.runJsonCommand.mockResolvedValueOnce([
      { ...dbService, volumes: [{ mountPath: dbVolumeMountPath }] },
    ]);

    await expect(
      assertDatabaseServiceHasVolume(dbService, dbVolumeMountPath, options),
    ).resolves.toBeUndefined();

    expect(mocks.runJsonCommand).toHaveBeenCalledExactlyOnceWith(
      mocks.railwayCli,
      ["service", "list", "--json"],
      RailwayCliServiceListSchema,
    );
  });

  test("throws when the service has no volume", async () => {
    mocks.runJsonCommand.mockResolvedValueOnce([{ ...dbService, volumes: [] }]);

    await expect(
      assertDatabaseServiceHasVolume(dbService, dbVolumeMountPath, options),
    ).rejects.toThrow(dbService.id);
  });
});
