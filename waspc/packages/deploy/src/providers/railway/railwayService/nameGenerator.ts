import {
  ClientServiceName,
  DbServiceName,
  RailwayProjectName,
  ServerServiceName,
} from "../brandedTypes";

export function generateRailwayClientServiceName(
  projectName: RailwayProjectName,
): ClientServiceName {
  return generateRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Client,
  ) as ClientServiceName;
}

export function generateRailwayServerServiceName(
  projectName: RailwayProjectName,
): ServerServiceName {
  return generateRailwayServiceName(
    projectName,
    ServiceWithSuffixedName.Server,
  ) as ServerServiceName;
}

function generateRailwayServiceName(
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

export function generateRailwayDbServiceName(): DbServiceName {
  // Railway doesn't allow us to choose the database service name.
  return "Postgres" as DbServiceName;
}
