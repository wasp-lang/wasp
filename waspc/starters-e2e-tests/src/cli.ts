import { program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };

export type WaspCliCommand = string;

interface StartersE2ETestsArgs {
  waspCliCommand: WaspCliCommand;
}

export function parseArgs(args: string[]): StartersE2ETestsArgs {
  const command = program
    .name("starters-e2e-tests")
    .description(
      "Run end-to-end tests for Wasp starter templates (except `ai` and `saas`)",
    )
    .version(packageJson.version)
    .requiredOption(
      "--wasp-cli-command <command>",
      "Path or command name for the Wasp CLI executable to test against",
    )
    .parse(args);

  return command.opts();
}
