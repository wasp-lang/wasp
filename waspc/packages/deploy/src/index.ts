#!/usr/bin/env node

import { Command } from "commander";
import { $, syncProcessCwd } from "zx";
import { addFlyCommand } from "./providers/fly/index.js";

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

addFlyCommand(program);

program.parseAsync();
