#!/usr/bin/env node

import { Command } from 'commander'
import { addFlyCommand } from './providers/fly/index.js'

const program = new Command()

// TODO: Try to make the output from this script stand out some. Use chalk maybe?
program
  .name('wasp deploy')
  .description('CLI for deploying Wasp apps to various clouds')
  .version('0.0.1')
  .allowUnknownOption()

addFlyCommand(program)

program.parseAsync()
