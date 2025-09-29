import { Command, Option } from "commander";
import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import {
  assertValidWaspProject,
  assertWaspProjectDirIsAbsoluteAndPresent,
} from "../../common/waspProject.js";
import { cmd as cmdFn } from "./commands/cmd/cmd.js";
import { createDb as createDbFn } from "./commands/createDb/createDb.js";
import { deploy as deployFn } from "./commands/deploy/deploy.js";
import { launch as launchFn } from "./commands/launch/launch.js";
import { setup as setupFn } from "./commands/setup/setup.js";
import { ContextOption } from "./CommonOps.js";
import { assertRegionIsValid, ensureFlyReady } from "./flyCli.js";
import { assertFlyTomlDirIsAbsoluteAndPresent } from "./tomlFile.js";

class FlyCommand extends Command {
  addBasenameArgument(): this {
    return this.argument(
      "<basename>",
      "base app name to use on Fly.io (must be unique)",
    );
  }
  addRegionArgument(): this {
    return this.argument(
      "<region>",
      "3 letter deployment region to use on Fly.io",
    );
  }
  addDbOptions(): this {
    return this.option(
      "--vm-size <vmSize>",
      "flyctl postgres create option",
      "shared-cpu-1x",
    )
      .option(
        "--initial-cluster-size <initialClusterSize>",
        "flyctl postgres create option",
        "1",
      )
      .option(
        "--volume-size <volumeSize>",
        "flyctl postgres create option",
        "1",
      )
      .option(
        "--db-image <dbImage>",
        "custom Docker image for the PostgreSQL database",
      );
  }
  addLocalBuildOption(): this {
    return this.option(
      "--build-locally",
      "build Docker containers locally instead of remotely",
      false,
    );
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

const flyLaunchCommand = makeFlyLaunchCommand();

export const flySetupCommand = makeFlySetupCommand();

export const createFlyDbCommand = makeCreateFlyDbCommand();

export const flyDeployCommand = makeFlyDeployCommand();

export const executeFlyCommand = makeExecuteFlyCommand();

export function createFlyCommand(): Command {
  const fly = new Command("fly")
    .description("Create and deploy Wasp apps on Fly.io")
    .addCommand(flyLaunchCommand)
    .addCommand(flySetupCommand)
    .addCommand(createFlyDbCommand)
    .addCommand(flyDeployCommand)
    .addCommand(executeFlyCommand)
    .allowUnknownOption();

  // Add global options and hooks to all commands.
  // Add these hooks before any command-specific ones so they run first.
  // NOTE: When we add another provider, consider pulling `--wasp-exe` and `--wasp-project-dir`
  // up as a global option that every provider can use (if possible).
  fly.commands.forEach((cmd) => {
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
      .option(
        "--fly-toml-dir <dir>",
        "absolute path to dir where fly.toml files live",
      )
      .option("--org <org>", "Fly org to use (with commands that support it)")
      .hook("preAction", async (cmd) => {
        const { waspProjectDir, waspExe, flyTomlDir } = cmd.opts<{
          waspProjectDir: WaspProjectDir;
          waspExe: WaspCliExe;
          flyTomlDir?: string;
        }>();

        await ensureFlyReady();

        assertWaspProjectDirIsAbsoluteAndPresent(waspProjectDir);
        if (flyTomlDir !== undefined) {
          assertFlyTomlDirIsAbsoluteAndPresent(flyTomlDir);
        }
        await assertValidWaspProject(waspProjectDir, waspExe);
      });
  });

  // Add command-specific hooks.
  flyLaunchCommand.hook("preAction", (_thisCommand, actionCommand) =>
    assertRegionIsValid(actionCommand.args[1]),
  );
  flySetupCommand.hook("preAction", (_thisCommand, actionCommand) =>
    assertRegionIsValid(actionCommand.args[1]),
  );
  createFlyDbCommand.hook("preAction", (_thisCommand, actionCommand) =>
    assertRegionIsValid(actionCommand.args[0]),
  );

  return fly;
}

function makeFlyLaunchCommand(): Command {
  return new FlyCommand("launch")
    .description(
      "Launch a new app on Fly.io (calls setup, create-db, and deploy)",
    )
    .addBasenameArgument()
    .addRegionArgument()
    .addDbOptions()
    .addLocalBuildOption()
    .addSecretsOptions()
    .action(launchFn);
}

function makeFlySetupCommand(): Command {
  return new FlyCommand("setup")
    .description("Set up a new app on Fly.io (this does not deploy it)")
    .addBasenameArgument()
    .addRegionArgument()
    .addSecretsOptions()
    .action(setupFn);
}

function makeFlyDeployCommand(): Command {
  return new FlyCommand("deploy")
    .description("(Re-)Deploy existing app to Fly.io")
    .option("--skip-client", "do not deploy the web client")
    .option("--skip-server", "do not deploy the server")
    .addLocalBuildOption()
    .action(deployFn);
}

function makeExecuteFlyCommand(): Command {
  return new FlyCommand("cmd")
    .description("Run arbitrary flyctl commands for server or client")
    .argument("<cmd...>", "flyctl command to run in server/client context")
    .addOption(
      new Option("--context <context>", "client or server context")
        .choices(Object.values(ContextOption))
        .makeOptionMandatory(),
    )
    .action(cmdFn)
    .allowUnknownOption();
}

function makeCreateFlyDbCommand(): Command {
  return new FlyCommand("create-db")
    .description("Creates a Postgres DB and attaches it to the server app")
    .addRegionArgument()
    .addDbOptions()
    .action(createDbFn);
}
