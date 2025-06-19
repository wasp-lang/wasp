import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import {
  ClientServiceName,
  RailwayCliExe,
  ServerServiceName,
} from "../brandedTypes.js";
import { RailwayCliDomainSchema } from "../jsonOutputSchemas.js";

export enum ServiceUrlStatus {
  URL_CREATED = "URL_CREATED",
  URL_ALREADY_EXISTS = "URL_ALREADY_EXISTS",
}

export async function generateServiceUrl(
  serviceName: ClientServiceName | ServerServiceName,
  port: number,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<string> {
  const { status, output } = await getServiceUrlStatus(
    serviceName,
    port,
    options,
  );

  switch (status) {
    case ServiceUrlStatus.URL_CREATED:
      const { domain } = RailwayCliDomainSchema.parse(JSON.parse(output));
      return domain;
    case ServiceUrlStatus.URL_ALREADY_EXISTS:
      // NOTE: Dealing with Railway CLI output quirk that if the domain already exists,
      // the output is not in JSON format, but rather a string with the domain URL.
      return matchServiceUrl(output);

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function getServiceUrlStatus(
  serviceName: ClientServiceName | ServerServiceName,
  port: number,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<
  | {
      status: ServiceUrlStatus.URL_CREATED;
      output: string;
    }
  | {
      status: ServiceUrlStatus.URL_ALREADY_EXISTS;
      output: string;
    }
> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  const result = await railwayCli([
    "domain",
    "--service",
    serviceName,
    "--port",
    port.toString(),
    "--json",
  ]);

  if (result.exitCode !== 0) {
    throw new Error(`There was a problem getting a domain for ${serviceName}.`);
  }

  if (result.stdout.includes("Domains already exists on the service:")) {
    return {
      status: ServiceUrlStatus.URL_ALREADY_EXISTS,
      output: result.stdout,
    };
  } else {
    return {
      status: ServiceUrlStatus.URL_CREATED,
      output: result.stdout,
    };
  }
}

function matchServiceUrl(text: string): string {
  const match = text.match(/https:\/\/[^\s]*/);
  if (match === null) {
    throw new Error("Failed to get service domain");
  }
  return match[0];
}
