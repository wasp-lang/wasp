import { program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };

export type WaspCliCommand = string;

interface StartersHeadlessE2ETestsArgs {
  waspCliCommand: WaspCliCommand;
}

export function parseArgs(args: string[]): StartersHeadlessE2ETestsArgs {
  const command = program
    .name("starters-headless-e2e-tests")
    .description(
      "Run headless end-to-end tests for Wasp starter templates (except ai)",
    )
    .version(packageJson.version)
    .requiredOption(
      "--wasp-cli-command <command>",
      "Path or command name for the Wasp CLI executable to test against",
    )
    .parse(args);

  return command.opts();
}
