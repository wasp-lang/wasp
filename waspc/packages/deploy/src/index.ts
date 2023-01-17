#!/usr/bin/env node

import { Command } from 'commander'
import { addFlyCommand } from './providers/fly/index.js'

const program = new Command()

program
  .name('wasp deploy')
  .description('CLI for deploying Wasp apps to various clouds')
  .allowUnknownOption()

addFlyCommand(program)

program.parseAsync()
