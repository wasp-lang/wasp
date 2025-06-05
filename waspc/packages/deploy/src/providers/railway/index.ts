import { Command, Option } from "commander";
import {
  ensureWaspDirLooksRight,
  ensureWaspProjectDirInCmdIsAbsoluteAndPresent,
} from "../../helpers.js";
import { deploy as deployFn } from "./deploy/deploy.js";
import {
  ensureRailwayBasenameIsValid,
  ensureRailwayReady,
} from "./helpers/railwayCli.js";
import { launch as launchFn } from "./launch/launch.js";
import { setup as setupFn } from "./setup/setup.js";

class RailwayCommand extends Command {
  addBasenameArgument(): this {
    return this.argument("<basename>", "base app name to use on Railway");
  }
  addSecretsOptions(): this {
    function collect(value: string, previous: string[]) {
      return previous.concat([value]);
    }
    return this.option(
      "--server-secret <serverSecret>",
      "secret to set on the server app (of form FOO=BAR)",
      collect,
      [],
    ).option(
      "--client-secret <clientSecret>",
      "secret to set on the client app (of form FOO=BAR)",
      collect,
      [],
    );
  }
}

export const railwaySetupCommand = makeRailwaySetupCommand();

export const railwayDeployCommand = makeRailwayDeployCommand();

const railwayLaunchCommand = makeRailwayLaunchCommand();

export function addRailwayCommand(program: Command): void {
  const railway = program
    .command("railway")
    .description("Create and deploy Wasp apps on Railway")
    .addCommand(railwaySetupCommand)
    .addCommand(railwayDeployCommand)
    .addCommand(railwayLaunchCommand)
    .allowUnknownOption();

  // Add global options and hooks to all commands.
  // Add these hooks before any command-specific ones so they run first.
  // NOTE: When we add another provider, consider pulling `--wasp-exe` and `--wasp-project-dir`
  // up as a global option that every provider can use (if possible).
  railway.commands.forEach((cmd) => {
    cmd
      .addOption(
        new Option(
          "--wasp-exe <path>",
          "Wasp executable (either on PATH or absolute path)",
        )
          .hideHelp()
          .makeOptionMandatory(),
      )
      .addOption(
        new Option(
          "--wasp-project-dir <dir>",
          "absolute path to Wasp project dir",
        )
          .hideHelp()
          .makeOptionMandatory(),
      )
      .addOption(
        new Option(
          "--railway-exe <path>",
          "Railway command to run (either on PATH or absolute path)",
        )
          .hideHelp()
          .default("railway"),
      )
      .option("--skip-build", "do not run `wasp build` before the command")
      .hook("preAction", ensureRailwayBasenameIsValid)
      .hook("preAction", ensureRailwayReady)
      .hook("preAction", ensureWaspProjectDirInCmdIsAbsoluteAndPresent)
      .hook("preAction", ensureWaspDirLooksRight);
  });
}

function makeRailwaySetupCommand(): Command {
  return new RailwayCommand("setup")
    .description("Configure a new app on Railway")
    .addBasenameArgument()
    .addSecretsOptions()
    .option(
      "--existing-project-id [projectId]",
      "use existing project instead of creating a new one",
    )
    .action(setupFn);
}

function makeRailwayDeployCommand(): Command {
  return new RailwayCommand("deploy")
    .description("Deploys the app to Railway")
    .addBasenameArgument()
    .option("--skip-client", "do not deploy the web client")
    .option("--skip-server", "do not deploy the server")
    .action(deployFn);
}

function makeRailwayLaunchCommand(): Command {
  return new RailwayCommand("launch")
    .description("Launch a new app on Railway (calls setup and deploy)")
    .addBasenameArgument()
    .addSecretsOptions()
    .option(
      "--existing-project-id [projectId]",
      "use existing project instead of creating a new one",
    )
    .action(launchFn);
}
