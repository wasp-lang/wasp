import { Option, program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };

export type WaspCliCommand = string;

export const PACKAGE_MANAGERS = ["npm", "pnpm", "bun"] as const;
export type PackageManager = (typeof PACKAGE_MANAGERS)[number];

interface StartersE2ETestsArgs {
  waspCliCommand: WaspCliCommand;
  packageManager: PackageManager;
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
    .addOption(
      new Option(
        "--package-manager <name>",
        "Package manager of the projects the tests create",
      )
        .choices(PACKAGE_MANAGERS)
        .default("npm" satisfies PackageManager),
    )
    .parse(args);

  return command.opts();
}
