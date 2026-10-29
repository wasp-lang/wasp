import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspInfo } from "../../../common/terminal.js";
import { createCommandWithCwd, tryRunJsonCommand } from "../../../common/zx.js";
import {
  ClientServiceName,
  Port,
  RailwayCliExe,
  ServerServiceName,
} from "../brandedTypes.js";
import { RailwayCliDomainSchema } from "../jsonOutputSchemas.js";

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
  const result = await tryRunJsonCommand(
    railwayCli,
    ["domain", "--service", serviceName, "--port", port.toString(), "--json"],
    RailwayCliDomainSchema,
  );

  if (result === null) {
    throw new Error(`There was a problem getting a domain for ${serviceName}.`);
  }

  const { domains } = result;
  const domain = domains[0];

  if (domains.length > 1) {
    waspInfo(`Multiple domains detected, using the first one: ${domain}.`);
    waspInfo(
      'If you want to use a custom domain for the server, you should add the "--custom-server-url <url>" flag.',
    );
  }

  return domain;
}
