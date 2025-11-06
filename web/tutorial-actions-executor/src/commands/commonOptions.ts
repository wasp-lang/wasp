import { Option } from "@commander-js/extra-typings";

import type { WaspCliCommand } from "../waspCli";

export const waspCliCommandOption = new Option(
  "--wasp-cli-command <command>",
  "Wasp CLI command to use",
)
  .default("wasp" as WaspCliCommand)
  .argParser((value) => value as WaspCliCommand);

export const appNameOption = new Option(
  "--app-name <name>",
  "Name of the app to generate",
).makeOptionMandatory();

export const outputDirOption = new Option(
  "--output-dir <path>",
  "Directory where the app will be generated",
).makeOptionMandatory();

export const tutorialDirOption = new Option(
  "--tutorial-dir <path>",
  "Directory containing the tutorial MDX files",
).makeOptionMandatory();
