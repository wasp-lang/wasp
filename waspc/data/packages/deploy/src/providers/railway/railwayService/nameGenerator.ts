import { waspSays } from "../../../common/terminal.js";
import {
  ClientServiceName,
  DbServiceName,
  RailwayProjectName,
  ServerServiceName,
} from "../brandedTypes.js";
import { getRailwayEnvVarValueReference } from "../env.js";
import type { RailwayProject } from "../railwayProject/RailwayProject.js";

/**
 * Creates a Railway client service name by appending the "-client" suffix to the project name.
 */
export function createRailwayClientServiceName(
  projectName: RailwayProjectName,
): ClientServiceName {
  return createRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Client,
  ) as ClientServiceName;
}

/**
 * Creates a Railway server service name by appending the "-server" suffix to the project name.
 */
export function createRailwayServerServiceName(
  projectName: RailwayProjectName,
): ServerServiceName {
  return createRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Server,
  ) as ServerServiceName;
}

function createRailwayServiceName(
  projectName: RailwayProjectName,
  service: ServiceWithSuffixedName,
): string {
  const serviceNameSuffix = serviceNameSuffixes[service];
  return `${projectName}${serviceNameSuffix}`;
}

enum ServiceWithSuffixedName {
  Client = "Client",
  Server = "Server",
}

export const serviceNameSuffixes: Record<ServiceWithSuffixedName, string> = {
  [ServiceWithSuffixedName.Client]: "-client",
  [ServiceWithSuffixedName.Server]: "-server",
};

/**
 * Creates a Railway database service name by appending the "-db" suffix to the project name.
 * This matches Fly.io's pattern and ensures consistent naming across all services.
 */
export function createRailwayDbServiceName(
  projectName: RailwayProjectName,
): DbServiceName {
  return `${projectName}-db` as DbServiceName;
}

/**
 * Gets the database service name with fallback for legacy deployments.
 * New deployments use <project-name>-db, but old deployments may still use "Postgres".
 * If a legacy "Postgres" service is found, a warning is displayed to guide users on renaming.
 */
export function getDbServiceNameWithFallback(
  projectName: RailwayProjectName,
  project: RailwayProject,
): DbServiceName {
  const newDbServiceName = createRailwayDbServiceName(projectName);

  if (project.doesServiceExist(newDbServiceName)) {
    return newDbServiceName;
  }

  // Fallback to legacy "Postgres" name for existing deployments
  const legacyDbServiceName = "Postgres" as DbServiceName;
  if (project.doesServiceExist(legacyDbServiceName)) {
    const legacyRef = getRailwayEnvVarValueReference("DATABASE_URL", {
      serviceName: legacyDbServiceName,
    });
    const newRef = getRailwayEnvVarValueReference("DATABASE_URL", {
      serviceName: newDbServiceName,
    });
    waspSays(`
⚠️  Warning: Your database service is named "Postgres" (legacy naming).
   New deployments use "${newDbServiceName}".

   To rename your database service:
   1. Go to your Railway project dashboard
   2. Click on the "Postgres" service
   3. Go to Settings → rename to "${newDbServiceName}"
   4. Update your server's DATABASE_URL variable:
      Change: ${legacyRef}
      To: ${newRef}
  `);
    return legacyDbServiceName;
  }

  // No database exists yet, return the new standard name
  return newDbServiceName;
}
