/// <reference types="node" />

import type { Command } from "../command.ts";
import { appE2eTestCommands } from "./appE2eTests.ts";
import { buildCommands } from "./build.ts";
import { codeQualityCommands } from "./codeQuality.ts";
import { ghcidCommands } from "./ghcid.ts";
import { testCommands } from "./test.ts";
import { versionCommands } from "./version.ts";
import { waspAppCommands } from "./waspApp.ts";

export const commandGroups: Command[][] = [
  buildCommands,
  waspAppCommands,
  [...testCommands, ...appE2eTestCommands],
  ghcidCommands,
  codeQualityCommands,
  versionCommands,
];

export function findCommand(name: string): Command | undefined {
  return commandGroups.flat().find((command) => command.name === name);
}
