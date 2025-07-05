import {
  ClientServiceName,
  DbServiceName,
  ServerServiceName,
} from "../brandedTypes";

const railwayPublicDomainEnvVarName = "RAILWAY_PUBLIC_DOMAIN";
const railwayDatabaseUrlEnvVarName = "DATABASE_URL";

export function getRailwayPublicUrlEnvVarForService(
  serviceName?: ClientServiceName | ServerServiceName,
): string {
  return `https://${getRailwayEnvVarReference(`${serviceName}.${railwayPublicDomainEnvVarName}`)}`;
}

export function getRailwayPublicUrlEnvVarForSelf(): string {
  return `https://${getRailwayEnvVarReference(railwayPublicDomainEnvVarName)}`;
}

export function getRailwayDatabaseUrlEnvVar(
  serviceName: DbServiceName,
): string {
  return getRailwayEnvVarReference(
    `${serviceName}.${railwayDatabaseUrlEnvVarName}`,
  );
}

function getRailwayEnvVarReference(name: string): string {
  return "${{" + name + "}}";
}
