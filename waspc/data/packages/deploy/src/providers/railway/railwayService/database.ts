import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { DbServiceName, RailwayCliExe } from "../brandedTypes.js";
import { getRailwayEnvVarValueReference } from "../env.js";
import {
  RailwayCliService,
  RailwayCliServiceListSchema,
  RailwayCliServiceSchema,
} from "../jsonOutputSchemas.js";

export async function createDatabaseService({
  serviceName,
  imageSpec,
  railwayExe,
  waspProjectDir,
}: {
  serviceName: DbServiceName;
  imageSpec: {
    image: string;
    volumeMountPath: string;
  };
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}): Promise<RailwayCliService> {
  const options = { railwayExe, waspProjectDir };
  const dbService = await addDatabaseService(
    serviceName,
    imageSpec.image,
    imageSpec.volumeMountPath,
    options,
  );

  try {
    await addDatabaseVolume(dbService, imageSpec.volumeMountPath, options);
  } catch (volumeError) {
    await deleteIncompleteDatabaseService(dbService, volumeError, options);
    throw volumeError;
  }

  return dbService;
}

export async function assertDatabaseServiceHasVolume(
  dbService: RailwayCliService,
  dbVolumeMountPath: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  if (!(await hasDatabaseVolume(dbService, dbVolumeMountPath, options))) {
    throw new Error(
      [
        `Railway database service "${dbService.name}" (${dbService.id}) has no volume mounted at ${dbVolumeMountPath}.`,
        "Mounting a volume there now would hide the database's existing data, so Wasp won't do it automatically.",
        "Back up any existing data before changing the service in Railway. Then add the required volume and restore the backup, or remove the service before trying again.",
      ].join("\n"),
    );
  }
}

async function addDatabaseService(
  dbServiceName: DbServiceName,
  dbImage: string,
  dbVolumeMountPath: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<RailwayCliService> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  // Image-backed services don't get the variables from Railway's Postgres
  // template, so we configure them explicitly.
  const dbVariables = {
    POSTGRES_DB: "railway",
    POSTGRES_USER: "postgres",
    POSTGRES_PASSWORD: getRailwayEnvVarValueReference("secret()"),
    PORT: "5432",
    // PGDATA must be a subdirectory of the volume mount.
    PGDATA: `${dbVolumeMountPath}/pgdata`,
    DATABASE_URL: `postgresql://${getRailwayEnvVarValueReference("POSTGRES_USER")}:${getRailwayEnvVarValueReference("POSTGRES_PASSWORD")}@${getRailwayEnvVarValueReference("RAILWAY_PRIVATE_DOMAIN")}:${getRailwayEnvVarValueReference("PORT")}/${getRailwayEnvVarValueReference("POSTGRES_DB")}`,
  };
  const variableArgs = Object.entries(dbVariables).flatMap(([name, value]) => [
    "--variables",
    `${name}=${value}`,
  ]);

  const result = await railwayCli(
    [
      "add",
      ...["--service", dbServiceName],
      ...["--image", dbImage],
      ...variableArgs,
      "--json",
    ],
    { verbose: false },
  );
  return RailwayCliServiceSchema.parse(result.json());
}

async function addDatabaseVolume(
  dbService: RailwayCliService,
  dbVolumeMountPath: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  await railwayCli(
    [
      "volume",
      ...["--service", dbService.id],
      "add",
      ...["--mount-path", dbVolumeMountPath],
      "--json",
    ],
    { verbose: false },
  );
}

async function deleteIncompleteDatabaseService(
  dbService: RailwayCliService,
  volumeError: unknown,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  try {
    await railwayCli(
      ["service", "delete", ...["--service", dbService.id], "--yes", "--json"],
      { verbose: false },
    );
  } catch (cleanupError) {
    throw new Error(
      [
        `Wasp couldn't finish setting up Railway database service "${dbService.name}" (${dbService.id}).`,
        "Wasp also couldn't remove the incomplete service. Remove it from Railway before trying again.",
        `Volume error: ${getErrorMessage(volumeError)}`,
        `Cleanup error: ${getErrorMessage(cleanupError)}`,
      ].join("\n"),
    );
  }

  waspSays(`Removed incomplete database service "${dbService.name}".`);
}

async function hasDatabaseVolume(
  dbService: RailwayCliService,
  dbVolumeMountPath: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<boolean> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  const result = await railwayCli(["service", "list", "--json"], {
    verbose: false,
  });
  const services = RailwayCliServiceListSchema.parse(result.json());
  return services.some(
    (service) =>
      service.id === dbService.id &&
      service.volumes.some((volume) => volume.mountPath === dbVolumeMountPath),
  );
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
