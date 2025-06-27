import {
  ClientServiceName,
  DbServiceName,
  ServerServiceName,
} from "../brandedTypes";

const railwayPublicDomainEnvVarName = "RAILWAY_PUBLIC_DOMAIN";
const railwayDatabaseUrlEnvVarName = "DATABASE_URL";

export function getRailwayPublicUrlReferenceForService(
  serviceName?: ClientServiceName | ServerServiceName,
): string {
  return `https://${getRailwayEnvVarReference(`${serviceName}.${railwayPublicDomainEnvVarName}`)}`;
}

export function getRailwayPublicUrlReferenceForSelf(): string {
  return `https://${getRailwayEnvVarReference(railwayPublicDomainEnvVarName)}`;
}

export function getRailwayDatabaseUrlReference(
  serviceName: DbServiceName,
): string {
  return getRailwayEnvVarReference(
    `${serviceName}.${railwayDatabaseUrlEnvVarName}`,
  );
}

function getRailwayEnvVarReference(name: string): string {
  return "${{" + name + "}}";
}
