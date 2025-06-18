import {
  ClientServiceName,
  DbServiceName,
  ServerServiceName,
} from "../DeploymentInfo";

// Uses the special Railway env variable "RAILWAY_PUBLIC_DOMAIN"
// to reference the public domain of a service.
export function getRailwayPublicUrlReference(
  serviceName?: ClientServiceName | ServerServiceName,
): string {
  const publicDomainEnvVarName = serviceName
    ? `${serviceName}.RAILWAY_PUBLIC_DOMAIN`
    : "RAILWAY_PUBLIC_DOMAIN";

  return `https://${getRailwayEnvVarReference(publicDomainEnvVarName)}`;
}

// Uses the special Railway env variable "DATABASE_URL" to reference
// the database URL of a database service.
export function getRailwayDatabaseUrlReference(
  serviceName: DbServiceName,
): string {
  return getRailwayEnvVarReference(`${serviceName}.DATABASE_URL`);
}

function getRailwayEnvVarReference(name: string): string {
  return "${{" + name + "}}";
}
