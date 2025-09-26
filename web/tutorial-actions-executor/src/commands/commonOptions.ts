import { Option } from "@commander-js/extra-typings";

export const waspCliCommandOption = new Option(
  "--wasp-cli-command <command>",
  "Wasp CLI command to use",
).default("wasp");
