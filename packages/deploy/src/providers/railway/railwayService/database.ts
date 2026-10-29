import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd, runJsonCommand } from "../../../common/zx.js";
import { DbServiceName, RailwayCliExe } from "../brandedTypes.js";
import { getRailwayEnvVarValueReference } from "../env.js";
import {
  RailwayCliService,
  RailwayCliServiceListSchema,
  RailwayCliServiceSchema,
} from "../jsonOutputSchemas.js";
import {
  RailwayServiceInstance,
  setServiceInstanceImage,
  startServiceInstanceDeployment,
} from "./serviceInstance.js";

// Creating a service with an image immediately starts a deployment, and
// Postgres crashes when its volume isn't attached yet.
export async function createDatabaseService({
  serviceName,
  imageSpec,
  environmentId,
  railwayExe,
  waspProjectDir,
}: {
  serviceName: DbServiceName;
  imageSpec: {
    image: string;
    volumeMountPath: string;
  };
  environmentId: string;
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}): Promise<RailwayCliService> {
  const options = { railwayExe, waspProjectDir };
  const dbService = await addDatabaseServiceWithoutImage(
    serviceName,
    imageSpec.volumeMountPath,
    options,
  );
  const dbServiceInstance: RailwayServiceInstance = {
    serviceId: dbService.id,
    environmentId,
  };

  try {
    await addDatabaseVolume(dbService, imageSpec.volumeMountPath, options);
    await setServiceInstanceImage(dbServiceInstance, imageSpec.image, options);
    await startServiceInstanceDeployment(dbServiceInstance, options);
  } catch (setupError) {
    await deleteIncompleteDatabaseService(dbService, setupError, options);
    throw setupError;
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

async function addDatabaseServiceWithoutImage(
  dbServiceName: DbServiceName,
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

  return runJsonCommand(
    railwayCli,
    ["add", ...["--service", dbServiceName], ...variableArgs, "--json"],
    RailwayCliServiceSchema,
  );
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
  setupError: unknown,
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
        `Setup error: ${getErrorMessage(setupError)}`,
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

  const services = await runJsonCommand(
    railwayCli,
    ["service", "list", "--json"],
    RailwayCliServiceListSchema,
  );
  return services.some(
    (service) =>
      service.id === dbService.id &&
      service.volumes.some((volume) => volume.mountPath === dbVolumeMountPath),
  );
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
