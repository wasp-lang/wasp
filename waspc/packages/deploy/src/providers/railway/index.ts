import { Command, Option } from "commander";
import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import {
  assertValidWaspProject,
  assertWaspProjectDirIsAbsoluteAndPresent,
} from "../../common/waspProject.js";
import { RailwayCliExe, RailwayProjectName } from "./brandedTypes.js";
import { deploy as deployFn } from "./commands/deploy/index.js";
import { launch as launchFn } from "./commands/launch/launch.js";
import { setup as setupFn } from "./commands/setup/setup.js";
import {
  assertRailwayProjectNameIsValid,
  ensureRailwayCliReady,
} from "./railwayCli.js";

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
      .hook("preAction", async (cmd) => {
        const { waspProjectDir, waspExe, railwayExe } = cmd.opts<{
          waspProjectDir: WaspProjectDir;
          waspExe: WaspCliExe;
          railwayExe: RailwayCliExe;
        }>();
        const railwayProjectName = cmd.args[0] as RailwayProjectName;

        await ensureRailwayCliReady(railwayExe);

        assertRailwayProjectNameIsValid(railwayProjectName);
        assertWaspProjectDirIsAbsoluteAndPresent(waspProjectDir);
        await assertValidWaspProject(waspProjectDir, waspExe);
      });
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
    .option(
      "--workspace [workspace]",
      "the Railway workspace to use if a new project needs to be created (if not provided, will ask interactively)",
    )
    .action(setupFn);
}

function makeRailwayDeployCommand(): Command {
  return new RailwayCommand("deploy")
    .description("Deploys the app to Railway")
    .addProjectNameArgument()
    .option("--skip-client", "do not deploy the web client")
    .option("--skip-server", "do not deploy the server")
    .option(
      "--existing-project-id [projectId]",
      "use existing project for deployment",
    )
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
    .option(
      "--workspace [workspace]",
      "the Railway workspace to use if a new project needs to be created (if not provided, will ask interactively)",
    )
    .action(launchFn);
}
