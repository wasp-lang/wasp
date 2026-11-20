import {
  ClientServiceName,
  DbServiceName,
  RailwayProjectName,
  ServerServiceName,
} from "../brandedTypes.js";

export function createRailwayClientServiceName(
  projectName: RailwayProjectName,
): ClientServiceName {
  return createRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Client,
  ) as ClientServiceName;
}

export function createRailwayServerServiceName(
  projectName: RailwayProjectName,
): ServerServiceName {
  return createRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Server,
  ) as ServerServiceName;
}

export function createRailwayDbServiceName(): DbServiceName {
  // Kept as "Postgres" for compatibility with databases created by earlier
  // Wasp versions.
  return "Postgres" as DbServiceName;
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
