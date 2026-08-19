import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { createCommandWithCwd, runJsonCommand } from "../../../common/zx.js";
import { RailwayCliExe } from "../brandedTypes.js";
import { RailwayCliEnvironmentListSchema } from "../jsonOutputSchemas.js";

// `railway status --json` returns all of the project's environments without
// saying which one is linked, so we ask `railway environment list` instead.
export async function getLinkedEnvironmentId(options: {
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}): Promise<string> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  const { environments } = await runJsonCommand(
    railwayCli,
    ["environment", "list", "--json"],
    RailwayCliEnvironmentListSchema,
  );
  const linkedEnvironment = environments.find(
    (environment) => environment.isLinked,
  );
  // We don't expect this to happen in a normally provisioned Railway project:
  // `railway init` creates a default "production" environment and links it,
  // and `railway link` always selects an environment as well.
  if (linkedEnvironment === undefined) {
    const environmentNames = environments
      .map((environment) => environment.name)
      .join(", ");
    throw new Error(
      `No Railway environment is linked to this directory. Run \`railway environment\` to link one of: ${environmentNames}.`,
    );
  }
  return linkedEnvironment.id;
}
