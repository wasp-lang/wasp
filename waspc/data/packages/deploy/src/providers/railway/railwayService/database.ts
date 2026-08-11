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

const databaseVolumeMountPath = "/var/lib/postgresql/data";
// PGDATA must be a subdirectory of the volume mount.
const databasePgDataPath = `${databaseVolumeMountPath}/pgdata`;

export async function createDatabaseServiceWithVolume(
  dbServiceName: DbServiceName,
  dbImage: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<RailwayCliService> {
  const dbService = await createDatabaseService(
    dbServiceName,
    dbImage,
    options,
  );

  try {
    await addDatabaseVolume(dbService, options);
  } catch (volumeError) {
    return rollbackDatabaseService(dbService, volumeError, options);
  }

  return dbService;
}

export async function assertDatabaseServiceHasVolume(
  dbService: RailwayCliService,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  if (await hasDatabaseVolume(dbService, options)) {
    return;
  }

  throw new Error(
    [
      `Railway database service "${dbService.name}" (${dbService.id}) has no volume mounted at ${databaseVolumeMountPath}.`,
      "Mounting a volume there now would hide the database's existing data, so Wasp won't do it automatically.",
      "Back up any existing data before changing the service in Railway. Then add the required volume and restore the backup, or remove the service before trying again.",
    ].join("\n"),
  );
}

async function createDatabaseService(
  dbServiceName: DbServiceName,
  dbImage: string,
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
  const result = await railwayCli(
    [
      "add",
      ...["--service", dbServiceName],
      ...["--image", dbImage],
      ...["--variables", "POSTGRES_DB=railway"],
      ...["--variables", "POSTGRES_USER=postgres"],
      ...[
        "--variables",
        `POSTGRES_PASSWORD=${getRailwayEnvVarValueReference("secret()")}`,
      ],
      ...["--variables", "PORT=5432"],
      ...["--variables", `PGDATA=${databasePgDataPath}`],
      ...[
        "--variables",
        `DATABASE_URL=postgresql://${getRailwayEnvVarValueReference("POSTGRES_USER")}:${getRailwayEnvVarValueReference("POSTGRES_PASSWORD")}@${getRailwayEnvVarValueReference("RAILWAY_PRIVATE_DOMAIN")}:${getRailwayEnvVarValueReference("PORT")}/${getRailwayEnvVarValueReference("POSTGRES_DB")}`,
      ],
      "--json",
    ],
    { verbose: false },
  );
  return RailwayCliServiceSchema.parse(result.json());
}

async function addDatabaseVolume(
  dbService: RailwayCliService,
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
      ...["--mount-path", databaseVolumeMountPath],
      "--json",
    ],
    { verbose: false },
  );
}

async function hasDatabaseVolume(
  dbService: RailwayCliService,
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
      service.volumes.some(
        (volume) => volume.mountPath === databaseVolumeMountPath,
      ),
  );
}

async function rollbackDatabaseService(
  dbService: RailwayCliService,
  provisioningError: unknown,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<never> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  try {
    await railwayCli(
      ["service", "delete", ...["--service", dbService.id], "--yes", "--json"],
      { verbose: false },
    );
  } catch (rollbackError) {
    throw new Error(
      [
        `Wasp couldn't finish setting up Railway database service "${dbService.name}" (${dbService.id}).`,
        "Wasp also couldn't remove the incomplete service. Remove it from Railway before trying again.",
        `Setup error: ${getErrorMessage(provisioningError)}`,
        `Cleanup error: ${getErrorMessage(rollbackError)}`,
      ].join("\n"),
    );
  }

  waspSays(`Removed incomplete database service "${dbService.name}".`);
  throw provisioningError;
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
