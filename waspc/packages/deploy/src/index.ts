#!/usr/bin/env node

import { Command } from 'commander';
import { addFlyCommand } from './providers/fly/index.js';
import { addRailwayCommand } from './providers/railway/index.js';
import { createGlobalOptions } from './providers/shared/options.js';

const program = new Command();

program
    .name('wasp deploy')
    .description('CLI for deploying Wasp apps to various clouds')
    .allowUnknownOption();

addFlyCommand(program);
addRailwayCommand(program);

// add global options to all subcommands
program.commands.forEach((cmd) => {
    cmd.commands.forEach((subCmd) => createGlobalOptions(subCmd));
});

program.parseAsync();
