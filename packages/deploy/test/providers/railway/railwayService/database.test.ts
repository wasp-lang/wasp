import { beforeEach, describe, expect, test, vi } from "vitest";
import { WaspProjectDir } from "../../../../src/common/brandedTypes.js";
import {
  DbServiceName,
  RailwayCliExe,
} from "../../../../src/providers/railway/brandedTypes.js";
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
const environmentId = "environment-id";
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
  environmentId,
  ...options,
};
const volumeError = new Error("Failed to create volume");

beforeEach(() => {
  mocks.railwayCli.mockReset();
  mocks.runJsonCommand.mockReset();
});

describe("createDatabaseService", () => {
  test("uses the configured mount path for PGDATA and the volume, and the configured image", async () => {
    mocks.runJsonCommand
      .mockResolvedValueOnce(dbService)
      .mockResolvedValueOnce({ data: { serviceInstanceUpdate: true } })
      .mockResolvedValueOnce({ data: { serviceInstanceDeployV2: "depl-id" } });
    mocks.railwayCli.mockResolvedValueOnce({});

    await expect(
      createDatabaseService(createDatabaseServiceParams),
    ).resolves.toEqual(dbService);

    expect(mocks.runJsonCommand).toHaveBeenNthCalledWith(
      1,
      mocks.railwayCli,
      expect.arrayContaining([`PGDATA=${dbVolumeMountPath}/pgdata`]),
      expect.anything(),
    );
    expect(mocks.railwayCli).toHaveBeenCalledExactlyOnceWith(
      expect.arrayContaining(["--mount-path", dbVolumeMountPath]),
      { verbose: false },
    );
    expect(mocks.runJsonCommand).toHaveBeenNthCalledWith(
      2,
      mocks.railwayCli,
      expect.arrayContaining([
        JSON.stringify({ input: { source: { image: dbImage } } }),
      ]),
      expect.anything(),
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

  test("deletes the incomplete service when setting the image fails", async () => {
    const imageError = new Error("Failed to set image");
    mocks.runJsonCommand
      .mockResolvedValueOnce(dbService)
      .mockRejectedValueOnce(imageError);
    mocks.railwayCli.mockResolvedValueOnce({}).mockResolvedValueOnce({});

    await expect(
      createDatabaseService(createDatabaseServiceParams),
    ).rejects.toBe(imageError);

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
  });

  test("throws when the service has no volume", async () => {
    mocks.runJsonCommand.mockResolvedValueOnce([{ ...dbService, volumes: [] }]);

    await expect(
      assertDatabaseServiceHasVolume(dbService, dbVolumeMountPath, options),
    ).rejects.toThrow(dbService.id);
  });
});
