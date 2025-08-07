import { Argument, program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };

export type WaspCliCommand = string;

type StartersHeadlessE2ETestsArgs = {
  waspCliCommand: WaspCliCommand;
};

export function parseArgs(args: string[]): StartersHeadlessE2ETestsArgs {
  const command = program
    .name("starters-headless-e2e-tests")
    .description(
      "Run headless end-to-end tests for Wasp starter templates (except ai)",
    )
    .version(packageJson.version)
    .addArgument(
      new Argument(
        "wasp-cli-command",
        "Path or command name for the Wasp CLI executable to test against",
      ).argRequired(),
    )
    .parse(args);

  const [waspCliCommand] = command.processedArgs;

  return { waspCliCommand };
}
