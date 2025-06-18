import { Command, Option } from "commander";
import { WaspCliExe, WaspProjectDir } from "../../common/cliArgs.js";
import {
  assertValidWaspProject,
  assertWaspProjectDirInCmdIsAbsoluteAndPresent,
} from "../../common/waspProject.js";
import { RailwayCliExe } from "./CommonOptions.js";
import { deploy as deployFn } from "./deploy/index.js";
import { RailwayProjectName } from "./DeploymentInfo.js";
import {
  assertRailwayProjectNameIsValid,
  ensureRailwayReady,
} from "./helpers/railwayCli.js";
import { launch as launchFn } from "./launch/launch.js";
import { setup as setupFn } from "./setup/setup.js";

class RailwayCommand extends Command {
  addProjectNameArgument(): this {
    return this.argument("<project-name>", "project name to use on Railway");
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

export function createRailwayCommand(): Command {
  const railway = new Command("railway")
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
      .hook("preAction", (cmd) =>
        ensureRailwayReady(cmd.opts().railwayExe as RailwayCliExe),
      )
      .hook("preAction", (cmd) =>
        assertRailwayProjectNameIsValid(cmd.args[0] as RailwayProjectName),
      )
      .hook("preAction", (cmd) =>
        assertWaspProjectDirInCmdIsAbsoluteAndPresent(
          cmd.opts().waspProjectDir as WaspProjectDir,
        ),
      )
      .hook("preAction", (cmd) =>
        assertValidWaspProject(
          cmd.opts().waspProjectDir as WaspProjectDir,
          cmd.opts().waspExe as WaspCliExe,
        ),
      );
  });

  return railway;
}

function makeRailwaySetupCommand(): Command {
  return new RailwayCommand("setup")
    .description("Configure a new app on Railway")
    .addProjectNameArgument()
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
    .addProjectNameArgument()
    .option("--skip-client", "do not deploy the web client")
    .option("--skip-server", "do not deploy the server")
    .action(deployFn);
}

function makeRailwayLaunchCommand(): Command {
  return new RailwayCommand("launch")
    .description("Launch a new app on Railway (calls setup and deploy)")
    .addProjectNameArgument()
    .addSecretsOptions()
    .option(
      "--existing-project-id [projectId]",
      "use existing project instead of creating a new one",
    )
    .action(launchFn);
}
