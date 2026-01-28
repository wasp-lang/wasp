import assert from "node:assert";
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

    const REACT_APP_API_URL = process.env.REACT_APP_API_URL;
    if (REACT_APP_API_URL) {
      assert(
        domains.includes(REACT_APP_API_URL),
        `The domain specified in REACT_APP_API_URL (${REACT_APP_API_URL}) does not match any of the domains returned by Railway CLI: ${domains.join(", ")}`,
      );

      waspInfo(
        `Using domain from REACT_APP_API_URL environment variable: ${REACT_APP_API_URL}`,
      );

      return REACT_APP_API_URL;
    } else {
      const domain = domains[0];

      if (domains.length > 1) {
        waspInfo(`Multiple domains detected, using the first one: ${domain}.`);
        waspInfo(
          "If you configured a custom domain for the server, you should run the command with an env variable: REACT_APP_API_URL=https://serverUrl.com <command>",
        );
      }

      return domain;
    }
  }
}

function extractServiceUrlFromString(text: string): string {
  const match = text.match(/https:\/\/[^\s]*/);
  if (match === null) {
    throw new Error("Failed to get service domain");
  }
  return match[0];
}
