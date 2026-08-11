import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { DbServiceName } from "../brandedTypes.js";
import { getRailwayEnvVarValueReference } from "../env.js";
import {
  RailwayCliService,
  RailwayCliServiceListSchema,
  RailwayCliServiceSchema,
} from "../jsonOutputSchemas.js";
import { RailwayCliOptions } from "../railwayCli.js";

const databaseVolumeMountPath = "/var/lib/postgresql/data";
// PGDATA must be a subdirectory of the volume mount because initdb refuses
// to initialize into the non-empty mount root.
const databasePgDataPath = `${databaseVolumeMountPath}/pgdata`;

type RailwayCli = ReturnType<typeof createCommandWithCwd>;

export async function createDatabaseServiceWithVolume(
  dbServiceName: DbServiceName,
  dbImage: string,
  options: RailwayCliOptions,
): Promise<RailwayCliService> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  // Image-backed services don't get the variables from Railway's Postgres
  // template, so we configure them explicitly.
  const createResult = await railwayCli(
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
  const dbService = RailwayCliServiceSchema.parse(createResult.json());

  if (dbService.name !== dbServiceName) {
    return rollbackDatabaseService(
      railwayCli,
      dbService,
      new Error(
        `Railway created database service "${dbService.name}" instead of "${dbServiceName}".`,
      ),
    );
  }

  try {
    // Adding the volume triggers a second deployment on top of the one
    // `railway add` started. `--service` must be the service ID: `railway
    // volume` resolves services by ID only.
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
  } catch (volumeError) {
    // Railway can report a failure even though it created the volume, so
    // confirm the volume is really missing before tearing the service down.
    if (
      await hasExpectedDatabaseVolume(railwayCli, dbService).catch(() => false)
    ) {
      return dbService;
    }

    return rollbackDatabaseService(railwayCli, dbService, volumeError);
  }

  return dbService;
}

export async function assertDatabaseServiceHasVolume(
  dbService: RailwayCliService,
  options: RailwayCliOptions,
): Promise<void> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  if (await hasExpectedDatabaseVolume(railwayCli, dbService)) {
    return;
  }

  throw new Error(
    [
      `Railway database service "${dbService.name}" (${dbService.id}) has no volume mounted at ${databaseVolumeMountPath}.`,
      "Wasp won't add an empty volume automatically because that could hide existing data.",
      "Back up any existing data before changing the service in Railway. Then add the required volume and restore the backup, or remove the service before trying again.",
    ].join("\n"),
  );
}

async function hasExpectedDatabaseVolume(
  railwayCli: RailwayCli,
  dbService: RailwayCliService,
): Promise<boolean> {
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
  railwayCli: RailwayCli,
  dbService: RailwayCliService,
  provisioningError: unknown,
): Promise<never> {
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
