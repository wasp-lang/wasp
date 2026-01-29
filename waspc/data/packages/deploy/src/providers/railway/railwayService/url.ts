import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspInfo } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import {
  ClientServiceName,
  Port,
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
  port: Port,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<string> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  const result = await railwayCli(
    ["domain", "--service", serviceName, "--port", port.toString(), "--json"],
    {
      nothrow: true,
    },
  );

  if (result.exitCode !== 0) {
    throw new Error(`There was a problem getting a domain for ${serviceName}.`);
  }

  // NOTE: Railway CLI 4.11.2 fixed a typo and we want to stay backwards compatible
  const domainAlreadyExistsPattern = /Domains already exists? on the service:/;
  if (domainAlreadyExistsPattern.test(result.stdout)) {
    return extractServiceUrlFromString(result.stdout);
  } else {
    const { domains } = RailwayCliDomainSchema.parse(result.json());
    const domain = domains[0];

    if (domains.length > 1) {
      waspInfo(`Multiple domains detected, using the first one: ${domain}.`);
      waspInfo(
        'If you want to use a custom domain for the server, you should add the "--custom-server-url <url>" flag.',
      );
    }

    return domain;
  }
}

function extractServiceUrlFromString(text: string): string {
  const match = text.match(/https:\/\/[^\s]*/);
  if (match === null) {
    throw new Error("Failed to get service domain");
  }
  return match[0];
}
