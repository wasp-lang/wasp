import { exit } from "process";

import { createCommandWithDirectory } from "../../../common/cli.js";
import { WaspProjectDir } from "../../../common/cliArgs.js";
import { waspSays } from "../../../common/output.js";
import { RailwayCliExe } from "../CommonOptions.js";
import { ClientServiceName, ServerServiceName } from "../DeploymentInfo.js";
import { RailwayCliDomainSchema } from "../jsonOutputSchemas.js";

export enum ServiceUrlStatus {
  URL_CREATED = "URL_CREATED",
  URL_ALREADY_EXISTS = "URL_ALREADY_EXISTS",
  UNKNOWN_ERROR = "UNKNOWN_ERROR",
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
    case ServiceUrlStatus.UNKNOWN_ERROR:
      waspSays(`There was a problem getting a domain for ${serviceName}.`);
      exit(1);
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
  | {
      status: ServiceUrlStatus.UNKNOWN_ERROR;
      output?: undefined;
    }
> {
  const railwayCli = createCommandWithDirectory(
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
    return {
      status: ServiceUrlStatus.UNKNOWN_ERROR,
    };
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
