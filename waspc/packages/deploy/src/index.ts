#!/usr/bin/env node

import { Command } from "commander";
import { $, syncProcessCwd } from "zx";
import { waspSays } from "./common/terminal.js";
import { createFlyCommand } from "./providers/fly/index.js";
import { createRailwayCommand } from "./providers/railway/index.js";

const program = new Command();

program
  .name("wasp deploy")
  .description("CLI for deploying Wasp apps to various clouds")
  .allowUnknownOption();

// We want to see the output of commands by default
$.verbose = true;

// Keeps the process.cwd() in sync with the internal $ current working directory
// if it is changed via cd().
syncProcessCwd();

program.addCommand(createFlyCommand());
program.addCommand(createRailwayCommand());

try {
  // parseAsync not only parses the command line arguments but also
  // executes the command and it will throw an error if the command fails.
  await program.parseAsync();
} catch (error) {
  if (error instanceof Error && error.message) {
    waspSays(error.message);
  }
  process.exit(1);
}
