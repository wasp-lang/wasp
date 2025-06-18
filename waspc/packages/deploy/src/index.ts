#!/usr/bin/env node

import { Command } from "commander";
import { $, syncProcessCwd } from "zx";
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

program.parseAsync();
