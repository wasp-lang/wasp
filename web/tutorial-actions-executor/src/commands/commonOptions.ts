import { Option } from "@commander-js/extra-typings";

import type { WaspCliCommand } from "../waspCli";

export const waspCliCommandOption = new Option(
  "--wasp-cli-command <command>",
  "Wasp CLI command to use",
)
  .default("wasp" as WaspCliCommand)
  .argParser((value) => value as WaspCliCommand);
