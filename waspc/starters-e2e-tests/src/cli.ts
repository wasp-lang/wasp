import { Argument, program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };

export type WaspCliCommand = string

type StartersE2ETestsArgs = {
  waspCliCommand: WaspCliCommand;
};

export function parseArgs(args: string[]): StartersE2ETestsArgs {
  const [_node, _script, ...userArgs] = args;

  const command = program
    .name("starters-e2e-tests")
    .description("Run end-to-end tests for Wasp starters")
    .version(packageJson.version)
    .addArgument(
      new Argument("wasp-cli-command", "Specify the Wasp CLI command to use")
        .argRequired()
    )
    .parse(userArgs, { from: "user" });

  const [ waspCliCommand ] = command.processedArgs;

  return { waspCliCommand };
}
