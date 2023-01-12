#!/usr/bin/env node

import { Command } from 'commander'
import { addFlyCommand } from './providers/fly/index.js'

const program = new Command()

program
  .name('wasp deploy')
  .description('CLI for deploying Wasp apps to various clouds')
  .version('0.0.1')

addFlyCommand(program)

program.parseAsync()
