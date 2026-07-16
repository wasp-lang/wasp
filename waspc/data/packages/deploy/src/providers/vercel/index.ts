import { Command, Option } from "commander";
import { $ } from "zx";

import { WaspProjectDir } from "../../common/brandedTypes.js";
import { waspSays } from "../../common/terminal.js";
import { deploy as deployFn } from "./deploy.js";
import { runVercelPreflightChecks } from "./preflight.js";
import { setup as setupFn, VercelSetupCmdOptions } from "./setup.js";

class VercelCommand extends Command {
  addProjectNameArgument(): this {
    return this.argument("<project-name>", "project name to use on Vercel");
  }
  addServerSecretsOption(): this {
    function collect(value: string, previous: string[]) {
      return previous.concat([value]);
    }
    return this.option(
      "--server-secret <serverSecret>",
      "secret to set on the server app (of form FOO=BAR)",
      collect,
      [],
    );
  }
  addAuthOptions(): this {
    return this.option(
      "--token <token>",
      "Vercel access token (defaults to the logged-in Vercel CLI session)",
    ).option("--scope <scope>", "Vercel team (slug or ID) to operate in");
  }
  addDatabaseOptions(): this {
    return this.option(
      "--db <db>",
      'managed database to provision via the Vercel Marketplace (only "neon" is supported)',
      "neon",
    )
      .option(
        "--database-url <url>",
        "skip database provisioning and use this connection string as DATABASE_URL",
      )
      .option(
        "--direct-url <url>",
        "non-pooled connection string for DIRECT_URL (only with --database-url; defaults to the --database-url value)",
      );
  }
}

export const vercelSetupCommand = makeVercelSetupCommand();

export const vercelDeployCommand = makeVercelDeployCommand();

const vercelLaunchCommand = makeVercelLaunchCommand();

export function createVercelCommand(): Command {
  const vercel = new Command("vercel")
    .description("Create and deploy Wasp apps on Vercel")
    .addCommand(vercelSetupCommand)
    .addCommand(vercelDeployCommand)
    .addCommand(vercelLaunchCommand)
    .allowUnknownOption();

  // Add global options and hooks to all commands.
  // Add these hooks before any command-specific ones so they run first.
  vercel.commands.forEach((cmd) => {
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
          "--vercel-exe <path>",
          "Vercel command to run (either on PATH or absolute path)",
        )
          .hideHelp()
          .default("vercel"),
      )
      .hook("preAction", (cmd) => {
        const { waspProjectDir } = cmd.opts<{ waspProjectDir: string }>();
        // Preflight (task-16): warn-not-block detection of Wasp project
        // features (pg-boss jobs, WebSockets) known to behave badly on
        // Vercel. This never throws and never blocks the command below.
        runVercelPreflightChecks(waspProjectDir as WaspProjectDir);
      })
      .hook("preAction", async (cmd) => {
        const { vercelExe, token } = cmd.opts<{
          vercelExe: string;
          token?: string;
        }>();
        await ensureVercelCliReady(vercelExe, token);
      });
  });

  return vercel;
}

async function ensureVercelCliReady(
  vercelExe: string,
  token?: string,
): Promise<void> {
  await ensureVercelCliInstalled(vercelExe);
  await ensureUserLoggedIn(vercelExe, token);
}

async function ensureVercelCliInstalled(vercelExe: string): Promise<void> {
  const result = await $({ nothrow: true, quiet: true })`${vercelExe} --version`;
  if (result.exitCode !== 0) {
    throw new Error(
      [
        "Failed to run the Vercel CLI. Most likely the Vercel CLI is not installed.",
        "Read how to install the Vercel CLI here: https://vercel.com/docs/cli",
      ].join("\n"),
    );
  }
}

async function ensureUserLoggedIn(
  vercelExe: string,
  token?: string,
): Promise<void> {
  const tokenArgs = token ? ["--token", token] : [];
  // quiet: zx's global verbose mode would otherwise echo the argv,
  // including the --token value, into the terminal/logs.
  const result = await $({
    nothrow: true,
    quiet: true,
  })`${vercelExe} whoami ${tokenArgs}`;
  if (result.exitCode !== 0) {
    throw new Error(
      token
        ? "Vercel did not accept the provided --token. Double check the token and its team access."
        : [
            "You are not logged in to the Vercel CLI.",
            `Log in with \`${vercelExe} login\` (or pass an access token via --token) and try again.`,
          ].join("\n"),
    );
  }
}

function makeVercelSetupCommand(): Command {
  return new VercelCommand("setup")
    .description("Configure a new app on Vercel")
    .addProjectNameArgument()
    .addServerSecretsOption()
    .addAuthOptions()
    .addDatabaseOptions()
    .action((...args: Parameters<typeof setupFn>) => setupFn(...args));
}

function makeVercelDeployCommand(): Command {
  return new VercelCommand("deploy")
    .description("Deploys the app to Vercel")
    .addProjectNameArgument()
    .addAuthOptions()
    .action((...args: Parameters<typeof deployFn>) => deployFn(...args));
}

function makeVercelLaunchCommand(): Command {
  return new VercelCommand("launch")
    .description("Launch a new app on Vercel (calls setup and deploy)")
    .addProjectNameArgument()
    .addServerSecretsOption()
    .addAuthOptions()
    .addDatabaseOptions()
    .action((...args: Parameters<typeof launchFn>) => launchFn(...args));
}

// Launch is setup followed by deploy (mirroring the Railway provider's
// launch composition): its options are setup's options, which are a
// superset of deploy's.
async function launchFn(
  appName: string,
  options: VercelSetupCmdOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Vercel!");
  await setupFn(appName, options);
  await deployFn(appName, options);
}
