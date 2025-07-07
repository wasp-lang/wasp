import {
  ClientServiceName,
  DbServiceName,
  RailwayProjectName,
  ServerServiceName,
} from "../brandedTypes";

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

export function createRailwayDbServiceName(): DbServiceName {
  // Railway doesn't allow us to choose the database service name.
  return "Postgres" as DbServiceName;
}
